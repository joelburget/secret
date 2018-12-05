{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | A 'Context' is used to define the capabilities of the Template Haskell code
-- that handles the inline C code. See the documentation of the data type for
-- more details.
--
-- In practice, a 'Context' will have to be defined for each library that
-- defines new C types, to allow the TemplateHaskell code to interpret said
-- types correctly.

module Language.C.Inline.Context
  ( -- * 'TypesTable'
    TypesTable
  , Purity(..)
  , convertType
  , CArray
  , typeNamesFromTypesTable

    -- * 'Context'
  , Context(..)
  , baseCtx
  , fptrCtx
  , funCtx
  , vecCtx
  , VecCtx(..)
  , bsCtx
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (mzero)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Data.Word (Word8, Word16, Word32, Word64)
import           Foreign.C.Types
import           Foreign.Ptr (Ptr, FunPtr)
import           Foreign.Storable (Storable)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.HashSet as HashSet

#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup (Semigroup, (<>))
#else
import           Data.Monoid ((<>))
#endif

#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
import           Data.Traversable (traverse)
#endif

import qualified Language.C.Types as C

-- | A mapping from 'C.TypeSpecifier's to Haskell types.  Needed both to
-- parse C types, and to convert them to Haskell types.
type TypesTable = Map.Map C.TypeSpecifier TH.TypeQ

-- | A data type to indicate whether the user requested pure or IO
-- function from Haskell
data Purity
  = Pure
  | IO
  deriving (Eq, Show)

-- | A 'Context' stores various information needed to produce the files with
-- the C code derived from the inline C snippets.
--
-- 'Context's can be composed with their 'Monoid' instance, where 'mappend' is
-- right-biased -- in @'mappend' x y@ @y@ will take precedence over @x@.
data Context = Context
  { ctxTypesTable :: TypesTable
    -- ^ Needed to convert C types to Haskell types.
  , ctxOutput :: Maybe (String -> String)
    -- ^ This function is used to post-process the functions generated
    -- from the C snippets.  Currently just used to specify C linkage
    -- when generating C++ code.
  , ctxForeignSrcLang :: Maybe TH.ForeignSrcLang
    -- ^ TH.LangC by default
  }


#if MIN_VERSION_base(4,9,0)
instance Semigroup Context where
  ctx2 <> ctx1 = Context
    { ctxTypesTable = ctxTypesTable ctx1 <> ctxTypesTable ctx2
    , ctxOutput = ctxOutput ctx1 <|> ctxOutput ctx2
    , ctxForeignSrcLang = ctxForeignSrcLang ctx1 <|> ctxForeignSrcLang ctx2
    }
#endif

instance Monoid Context where
  mempty = Context
    { ctxTypesTable = mempty
    , ctxOutput = Nothing
    , ctxForeignSrcLang = Nothing
    }

#if !MIN_VERSION_base(4,11,0)
  mappend ctx2 ctx1 = Context
    { ctxTypesTable = ctxTypesTable ctx1 <> ctxTypesTable ctx2
    , ctxOutput = ctxOutput ctx1 <|> ctxOutput ctx2
    , ctxForeignSrcLang = ctxForeignSrcLang ctx1 <|> ctxForeignSrcLang ctx2
    }
#endif

-- | Context useful to work with vanilla C. Used by default.
--
-- 'ctxTypesTable': converts C basic types to their counterparts in
-- "Foreign.C.Types".
baseCtx :: Context
baseCtx = mempty
  { ctxTypesTable = baseTypesTable
  }

baseTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
baseTypesTable = Map.fromList
  [ (C.Void, [t| () |])
  -- Types from Foreign.C.Types in the order in which they are presented there,
  -- along with its documentation's section headers.
  --
  -- Integral types
  , (C.Char Nothing, [t| CChar |])
  , (C.Char (Just C.Signed), [t| CSChar |])
  , (C.Char (Just C.Unsigned), [t| CUChar |])
  , (C.Short C.Signed, [t| CShort |])
  , (C.Short C.Unsigned, [t| CUShort |])
  , (C.Int C.Signed, [t| CInt |])
  , (C.Int C.Unsigned, [t| CUInt |])
  , (C.Long C.Signed, [t| CLong |])
  , (C.Long C.Unsigned, [t| CULong |])
  , (C.TypeName "ptrdiff_t", [t| CPtrdiff |])
  , (C.TypeName "size_t", [t| CSize |])
  , (C.TypeName "wchar_t", [t| CWchar |])
  , (C.TypeName "sig_atomic_t", [t| CSigAtomic |])
  , (C.LLong C.Signed, [t| CLLong |])
  , (C.LLong C.Unsigned, [t| CULLong |])
  , (C.TypeName "intptr_t", [t| CIntPtr |])
  , (C.TypeName "uintptr_t", [t| CUIntPtr |])
  , (C.TypeName "intmax_t", [t| CIntMax |])
  , (C.TypeName "uintmax_t", [t| CUIntMax |])
  -- Numeric types
  , (C.TypeName "clock_t", [t| CClock |])
  , (C.TypeName "time_t", [t| CTime |])
  , (C.TypeName "useconds_t", [t| CUSeconds |])
  , (C.TypeName "suseconds_t", [t| CSUSeconds |])
  -- Floating types
  , (C.Float, [t| CFloat |])
  , (C.Double, [t| CDouble |])
  -- Other types
  , (C.TypeName "FILE", [t| CFile |])
  , (C.TypeName "fpos_t", [t| CFpos |])
  , (C.TypeName "jmp_buf", [t| CJmpBuf |])
  -- Types from stdint.h that can be statically mapped to their Haskell
  -- equivalents. Excludes int_fast*_t and int_least*_t and the corresponding
  -- unsigned types, since their sizes are platform-specific.
  , (C.TypeName "int8_t", [t| Int8 |])
  , (C.TypeName "int16_t", [t| Int16 |])
  , (C.TypeName "int32_t", [t| Int32 |])
  , (C.TypeName "int64_t", [t| Int64 |])
  , (C.TypeName "uint8_t", [t| Word8 |])
  , (C.TypeName "uint16_t", [t| Word16 |])
  , (C.TypeName "uint32_t", [t| Word32 |])
  , (C.TypeName "uint64_t", [t| Word64 |])
  ]

-- | An alias for 'Ptr'.
type CArray = Ptr

------------------------------------------------------------------------
-- Type conversion

-- | Given a 'Context', it uses its 'ctxTypesTable' to convert
-- arbitrary C types.
convertType
  :: Purity
  -> TypesTable
  -> C.Type C.CIdentifier
  -> TH.Q (Maybe TH.Type)
convertType purity cTypes = runMaybeT . go
  where
    goDecl = go . C.parameterDeclarationType

    go :: C.Type C.CIdentifier -> MaybeT TH.Q TH.Type
    go cTy = case cTy of
      C.TypeSpecifier _specs cSpec ->
        case Map.lookup cSpec cTypes of
          Nothing -> mzero
          Just ty -> lift ty
      C.Ptr _quals (C.Proto retType pars) -> do
        hsRetType <- go retType
        hsPars <- mapM goDecl pars
        lift [t| FunPtr $(buildArr hsPars hsRetType) |]
      C.Ptr _quals cTy' -> do
        hsTy <- go cTy'
        lift [t| Ptr $(return hsTy) |]
      C.Array _mbSize cTy' -> do
        hsTy <- go cTy'
        lift [t| CArray $(return hsTy) |]
      C.Proto _retType _pars -> do
        -- We cannot convert standalone prototypes
        mzero

    buildArr [] hsRetType =
      case purity of
        Pure -> [t| $(return hsRetType) |]
        IO -> [t| IO $(return hsRetType) |]
    buildArr (hsPar : hsPars) hsRetType =
      [t| $(return hsPar) -> $(buildArr hsPars hsRetType) |]

typeNamesFromTypesTable :: TypesTable -> C.TypeNames
typeNamesFromTypesTable cTypes = HashSet.fromList
  [ id' | C.TypeName id' <- Map.keys cTypes ]

-- | This 'Context' adds support for 'ForeignPtr' arguments. It adds a unique
-- marshaller called @fptr-ptr@. For example, @$fptr-ptr:(int *x)@ extracts the
-- bare C pointer out of foreign pointer @x@.
fptrCtx :: Context
fptrCtx = mempty

funCtx :: Context
funCtx = mempty

vecCtx :: Context
vecCtx = mempty

-- | Type class used to implement the anti-quoters in 'vecCtx'.
class VecCtx a where
  type VecCtxScalar a :: *

  vecCtxLength :: a -> Int
  vecCtxUnsafeWith :: a -> (Ptr (VecCtxScalar a) -> IO b) -> IO b

instance Storable a => VecCtx (V.Vector a) where
  type VecCtxScalar (V.Vector a) = a

  vecCtxLength = V.length
  vecCtxUnsafeWith = V.unsafeWith

instance Storable a => VecCtx (VM.IOVector a) where
  type VecCtxScalar (VM.IOVector a) = a

  vecCtxLength = VM.length
  vecCtxUnsafeWith = VM.unsafeWith

bsCtx :: Context
bsCtx = mempty
