{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MonoLocalBinds #-}

module Language.C.Inline.Internal
    ( -- * Context handling
      setContext
    , getContext

      -- * Emitting and invoking C code
      --
      -- | The functions in this section let us access more the C file
      -- associated with the current module.  They can be used to build
      -- additional features on top of the basic machinery.  All of
      -- @inline-c@ is based upon the functions defined here.

      -- ** Emitting C code
    , emitVerbatim

      -- ** Inlining C code
      -- $embedding
    , Code(..)
    , inlineCode
    , inlineExp
    , inlineItems

      -- * Parsing
      --
      -- | These functions are used to parse the anti-quotations.  They're
      -- exposed for testing purposes, you really should not use them.
    , ParameterType(..)
    , parseTypedC
    , runParserInQ
    , splitTypedC

      -- * Utility functions for writing quasiquoters
    , genericQuote
    ) where

import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Data.Foldable (forM_)
import           Data.Maybe (fromMaybe)
import           Data.Traversable (for)
import           Data.Typeable (Typeable)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import           System.IO.Unsafe (unsafePerformIO)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec
import qualified Text.Parser.Char as Parser
import qualified Text.Parser.Combinators as Parser
import qualified Text.Parser.Token as Parser
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.List as L
import qualified Data.Char as C


import           Language.C.Inline.Context
import           Language.C.Inline.FunPtr
import           Language.C.Inline.HaskellIdentifier
import qualified Language.C.Types as C

data ModuleState = ModuleState
  { msContext :: Context
  , msGeneratedNames :: Int
  , msFileChunks :: [String]
  } deriving (Typeable)

getModuleState :: TH.Q (Maybe ModuleState)
getModuleState = TH.getQ

putModuleState :: ModuleState -> TH.Q ()
putModuleState = TH.putQ


-- | Make sure that 'moduleStatesVar' and the respective C file are up
--   to date.
initialiseModuleState
  :: Maybe Context
  -- ^ The 'Context' to use if we initialise the module.  If 'Nothing',
  -- 'baseCtx' will be used.
  -> TH.Q Context
initialiseModuleState mbContext = do
  mbModuleState <- getModuleState
  case mbModuleState of
    Just moduleState -> return (msContext moduleState)
    Nothing -> do
      -- Add hook to add the file
      TH.addModFinalizer $ do
        mbMs <- getModuleState
        ms <- case mbMs of
          Nothing -> fail "inline-c: ModuleState not present (initialiseModuleState)"
          Just ms -> return ms
        let lang = fromMaybe TH.LangC (ctxForeignSrcLang context)
        TH.addForeignFile lang (concat (reverse (msFileChunks ms)))
      let moduleState = ModuleState
            { msContext = context
            , msGeneratedNames = 0
            , msFileChunks = mempty
            }
      putModuleState moduleState
      return context
  where
    context = fromMaybe baseCtx mbContext

-- | Gets the current 'Context'.  Also makes sure that the current
-- module is initialised.
getContext :: TH.Q Context
getContext = initialiseModuleState Nothing

modifyModuleState :: (ModuleState -> (ModuleState, a)) -> TH.Q a
modifyModuleState f = do
  mbModuleState <- getModuleState
  case mbModuleState of
    Nothing -> fail "inline-c: ModuleState not present (modifyModuleState)"
    Just ms -> do
      let (ms', x) = f ms
      putModuleState ms'
      return x

-- $context
--
-- The inline C functions ('cexp', 'c', etc.) need a 'Context' to
-- operate.  Said context can be explicitely set with 'setContext'.
-- Otherwise, at the first usage of one of the TH functions in this
-- module the 'Context' is implicitely set to 'baseCtx'.

-- | Sets the 'Context' for the current module.  This function, if
-- called, must be called before any of the other TH functions in this
-- module.  Fails if that's not the case.
setContext :: Context -> TH.Q ()
setContext ctx = do
  mbModuleState <- getModuleState
  forM_ mbModuleState $ \_ms ->
    fail "inline-c: The module has already been initialised (setContext)."
  void $ initialiseModuleState $ Just ctx

bumpGeneratedNames :: TH.Q Int
bumpGeneratedNames = do
  modifyModuleState $ \ms ->
    let c' = msGeneratedNames ms
    in (ms{msGeneratedNames = c' + 1}, c')

------------------------------------------------------------------------
-- Emitting

-- | Simply appends some string to the module's C file.  Use with care.
emitVerbatim :: String -> TH.DecsQ
emitVerbatim s = do
  -- Make sure that the 'ModuleState' is initialized
  void (initialiseModuleState Nothing)
  let chunk = "\n" ++ s ++ "\n"
  modifyModuleState $ \ms ->
    (ms{msFileChunks = chunk : msFileChunks ms}, ())
  return []

------------------------------------------------------------------------
-- Inlining

-- $embedding
--
-- We use the 'Code' data structure to represent some C code that we
-- want to emit to the module's C file and immediately generate a
-- foreign call to.  For this reason, 'Code' includes both some C
-- definition, and enough information to be able to generate a foreign
-- call -- specifically the name of the function to call and the Haskell
-- type.
--
-- All the quasi-quoters work by constructing a 'Code' and calling
-- 'inlineCode'.

-- | Data type representing a list of C definitions with a typed and named entry
-- function.
--
-- We use it as a basis to inline and call C code.
data Code = Code
  { codeCallSafety :: TH.Safety
    -- ^ Safety of the foreign call.
  , codeType :: TH.TypeQ
    -- ^ Type of the foreign call.
  , codeFunName :: String
    -- ^ Name of the function to call in the code below.
  , codeDefs :: String
    -- ^ The C code.
  }

-- TODO use the #line CPP macro to have the functions in the C file
-- refer to the source location in the Haskell file they come from.
--
-- See <https://gcc.gnu.org/onlinedocs/cpp/Line-Control.html>.

-- | Inlines a piece of code inline.  The resulting 'TH.Exp' will have
-- the type specified in the 'codeType'.
--
-- In practice, this function outputs the C code to the module's C file,
-- and then inserts a foreign call of type 'codeType' calling the
-- provided 'codeFunName'.
--
-- Example:
--
-- @
-- c_add :: Int -> Int -> Int
-- c_add = $(inlineCode $ Code
--   TH.Unsafe                   -- Call safety
--   [t| Int -> Int -> Int |]    -- Call type
--   "francescos_add"            -- Call name
--   -- C Code
--   \"int francescos_add(int x, int y) { int z = x + y; return z; }\")
-- @
inlineCode :: Code -> TH.ExpQ
inlineCode Code{..} = do
  -- Write out definitions
  ctx <- getContext
  let out = fromMaybe id $ ctxOutput ctx
  void $ emitVerbatim $ out codeDefs
  -- Create and add the FFI declaration.
  ffiImportName <- uniqueFfiImportName
  dec <- TH.forImpD TH.CCall codeCallSafety codeFunName ffiImportName codeType
  TH.addTopDecls [dec]
  TH.varE ffiImportName

uniqueCName :: TH.Q String
uniqueCName = do
  -- The name looks like this:
  -- inline_c_MODULE_INDEX
  --
  -- Where:
  --  * MODULE is the module name but with _s instead of .s;
  --  * INDEX is a counter that keeps track of how many names we're generating
  --    for each module.
  --
  -- we previously also generated a hash from the contents of the
  -- C code because of problems when cabal recompiled but now this
  -- is not needed anymore since we use 'addDependentFile' to compile
  -- the C code.
  c' <- bumpGeneratedNames
  module_ <- TH.loc_module <$> TH.location
  let replaceDot '.' = '_'
      replaceDot c = c
  return $ "inline_c_" ++ map replaceDot module_ ++ "_" ++ show c'

-- | Same as 'inlineCItems', but with a single expression.
--
-- @
-- c_cos :: Double -> Double
-- c_cos = $(inlineExp
--   TH.Unsafe
--   [t| Double -> Double |]
--   (quickCParser_ \"double\" parseType)
--   [("x", quickCParser_ \"double\" parseType)]
--   "cos(x)")
-- @
inlineExp
  :: TH.Safety
  -- ^ Safety of the foreign call
  -> TH.TypeQ
  -- ^ Type of the foreign call
  -> C.Type C.CIdentifier
  -- ^ Return type of the C expr
  -> [(C.CIdentifier, C.Type C.CIdentifier)]
  -- ^ Parameters of the C expr
  -> String
  -- ^ The C expression
  -> TH.ExpQ
inlineExp callSafety type_ cRetType cParams cExp =
  inlineItems callSafety type_ cRetType cParams cItems
  where
    cItems = case cRetType of
      C.TypeSpecifier _quals C.Void -> cExp ++ ";"
      _ -> "return (" ++ cExp ++ ");"

-- | Same as 'inlineCode', but accepts a string containing a list of C
-- statements instead instead than a full-blown 'Code'.  A function
-- containing the provided statement will be automatically generated.
--
-- @
-- c_cos :: Double -> Double
-- c_cos = $(inlineItems
--   TH.Unsafe
--   [t| Double -> Double |]
--   (quickCParser_ \"double\" parseType)
--   [("x", quickCParser_ \"double\" parseType)]
--   "return cos(x);")
-- @
inlineItems
  :: TH.Safety
  -- ^ Safety of the foreign call
  -> TH.TypeQ
  -- ^ Type of the foreign call
  -> C.Type C.CIdentifier
  -- ^ Return type of the C expr
  -> [(C.CIdentifier, C.Type C.CIdentifier)]
  -- ^ Parameters of the C expr
  -> String
  -- ^ The C items
  -> TH.ExpQ
inlineItems callSafety type_ cRetType cParams cItems = do
  let mkParam (id', paramTy) = C.ParameterDeclaration (Just id') paramTy
  let proto = C.Proto cRetType (map mkParam cParams)
  funName <- uniqueCName
  cFunName <- case C.cIdentifierFromString funName of
    Left err -> fail $ "inlineItems: impossible, generated bad C identifier " ++
                       "funName:\n" ++ err
    Right x -> return x
  let decl = C.ParameterDeclaration (Just cFunName) proto
  let defs =
        prettyOneLine decl ++ " {\n" ++
        cItems ++ "\n}\n"
  inlineCode $ Code
    { codeCallSafety = callSafety
    , codeType = type_
    , codeFunName = funName
    , codeDefs = defs
    }

------------------------------------------------------------------------
-- Parsing

runParserInQ
  :: String -> C.TypeNames -> (forall m. C.CParser HaskellIdentifier m => m a) -> TH.Q a
runParserInQ s typeNames' p = do
  loc <- TH.location
  let (line, col) = TH.loc_start loc
  let parsecLoc = Parsec.newPos (TH.loc_filename loc) line col
  let p' = C.P $ lift (Parsec.setPosition parsecLoc) *> p <* lift Parser.eof
  case C.runCParser (haskellCParserContext typeNames') (TH.loc_filename loc) s p' of
    Left err -> do
      -- TODO consider prefixing with "error while parsing C" or similar
      fail $ show err
    Right res -> do
      return res

data ParameterType
  = Plain HaskellIdentifier                -- The name of the captured variable
  deriving (Show, Eq)

-- To parse C declarations, we're faced with a bit of a problem: we want
-- to parse the anti-quotations so that Haskell identifiers are
-- accepted, but we want them to appear only as the root of
-- declarations.  For this reason, we parse allowing Haskell identifiers
-- everywhere, and then we "purge" Haskell identifiers everywhere but at
-- the root.
parseTypedC
  :: forall m. C.CParser HaskellIdentifier m
  => m (C.Type C.CIdentifier)
  -- ^ Returns the return type, the captured variables, and the body.
parseTypedC = do
  -- Parse return type (consume spaces first)
  Parser.spaces
  ret <- purgeHaskellIdentifiers =<< C.parseType
  _ <- Parser.symbolic ';'
  pure ret

  where

    -- The @m@ is polymorphic because we use this both for the plain
    -- parser and the StateT parser we use above.  We only need 'fail'.
    purgeHaskellIdentifiers
      :: forall n. (Applicative n, Monad n)
      => C.Type HaskellIdentifier -> n (C.Type C.CIdentifier)
    purgeHaskellIdentifiers cTy = for cTy $ \hsIdent -> do
      let hsIdentS = unHaskellIdentifier hsIdent
      case C.cIdentifierFromString hsIdentS of
        Left err -> fail $ "Haskell identifier " ++ hsIdentS ++ " in illegal position" ++
                           "in C type\n" ++ pretty80 cTy ++ "\n" ++
                           "A C identifier was expected, but:\n" ++ err
        Right cIdent -> return cIdent

quoteCode
  :: (String -> TH.ExpQ)
  -- ^ The parser
  -> TH.QuasiQuoter
quoteCode p = TH.QuasiQuoter
  { TH.quoteExp  = p
  , TH.quotePat  = fail "inline-c: quotePat not implemented (quoteCode)"
  , TH.quoteType = fail "inline-c: quoteType not implemented (quoteCode)"
  , TH.quoteDec  = fail "inline-c: quoteDec not implemented (quoteCode)"
  }

genericQuote
  :: Purity
  -> (TH.TypeQ -> C.Type C.CIdentifier -> [(C.CIdentifier, C.Type C.CIdentifier)] -> String -> TH.ExpQ)
  -- ^ Function building an Haskell expression, see 'inlineExp' for
  -- guidance on the other args.
  -> TH.QuasiQuoter
genericQuote purity build = quoteCode $ \s -> do
    ctx <- getContext
    cType <-
      runParserInQ s (typeNamesFromTypesTable (ctxTypesTable ctx)) parseTypedC
    hsType <- cToHs ctx cType
    -- hsParams <- forM cParams $ \(_cId, cTy, parTy) -> do
    --   case parTy of
    --     Plain s' -> do
    --       hsTy <- cToHs ctx cTy
    --       let hsName = TH.mkName (unHaskellIdentifier s')
    --       hsExp <- [| \cont -> cont ($(TH.varE hsName) :: $(return hsTy)) |]
    --       return (hsTy, hsExp)
    let hsFunType = convertCFunSig hsType []
    let cParams' = [] -- [(cId, cTy) | (cId, cTy, _) <- cParams]
    ioCall <- buildFunCall ctx (build hsFunType cType cParams' "TODO") [] []
    -- If the user requested a pure function, make it so.
    case purity of
      Pure -> [| unsafePerformIO $(return ioCall) |]
      IO -> return ioCall
  where
    cToHs :: Context -> C.Type C.CIdentifier -> TH.TypeQ
    cToHs ctx cTy = do
      mbHsTy <- convertType purity (ctxTypesTable ctx) cTy
      case mbHsTy of
        Nothing -> fail $ "Could not resolve Haskell type for C type " ++ pretty80 cTy
        Just hsTy -> return hsTy

    buildFunCall :: Context -> TH.ExpQ -> [TH.Exp] -> [TH.Name] -> TH.ExpQ
    buildFunCall _ctx f [] args =
      foldl (\f' arg -> [| $f' $(TH.varE arg) |]) f args
    buildFunCall ctx f (hsExp : params) args =
       [| $(return hsExp) $ \arg ->
            $(buildFunCall ctx f params (args ++ ['arg]))
       |]

    convertCFunSig :: TH.Type -> [TH.Type] -> TH.TypeQ
    convertCFunSig retType params0 = do
      go params0
      where
        go [] =
          [t| IO $(return retType) |]
        go (paramType : params) = do
          [t| $(return paramType) -> $(go params) |]

splitTypedC :: String -> (String, String)
  -- ^ Returns the type and the body separately
splitTypedC s = (trim ty, case body of
                            [] -> []
                            r  -> r)
  where (ty, body) = span (/= '{') s
        trim x = L.dropWhileEnd C.isSpace (dropWhile C.isSpace x)

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Pretty a => a -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 (PP.pretty x)) ""

prettyOneLine :: PP.Pretty a => a -> String
prettyOneLine x = PP.displayS (PP.renderCompact (PP.pretty x)) ""
