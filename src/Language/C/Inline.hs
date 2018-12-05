{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Enable painless embedding of C code in Haskell code. If you're interested
-- in how to use the library, skip to the "Inline C" section. To build, read the
-- first two sections.
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified "Language.C.Inline" as C
-- @

module Language.C.Inline
  ( -- * GHCi
    -- $building

    -- * Contexts
    Context
  , baseCtx
  , fptrCtx
  , funCtx
  , vecCtx
  , bsCtx
  , context

    -- * Inline C
    -- $quoting
  , exp
  , pure
  , include
  , verbatim

    -- * C types re-exports
    --
    -- Re-export these to avoid errors when `inline-c` generates FFI calls GHC
    -- needs the constructors for those types.
  , module Foreign.C.Types
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Prelude hiding (exp)
#else
import           Prelude hiding (exp, pure)
#endif

import           Control.Monad (void)
import           Foreign.C.Types
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH

import           Language.C.Inline.Context
import           Language.C.Inline.Internal

-- $building
--
-- Currently @inline-c@ does not work in interpreted mode. However, GHCi
-- can still be used using the @-fobject-code@ flag. For speed, we
-- reccomend passing @-fobject-code -O0@, for example
--
-- @
-- stack ghci --ghci-options='-fobject-code -O0'
-- @
--
-- or
--
-- @
-- cabal repl --ghc-options='-fobject-code -O0'
-- @

------------------------------------------------------------------------
-- Quoting sugar

-- $quoting
--
-- The quasiquoters below are the main interface to this library, for inlining
-- C code into Haskell source files.
--
-- In general, quasiquoters are used like so:
--
-- @
-- [C.XXX| int { \<C code\> } |]
-- @
--
-- Where @C.XXX@ is one of the quasi-quoters defined in this section.
--
-- This syntax stands for a piece of typed C, decorated with a type:
--
-- * The first type to appear (@int@ in the example) is the type of said C code.
--
-- * The syntax of the @\<C code\>@ depends on on the quasi-quoter used, and the
--   anti-quoters available. The @exp@ quasi-quoter expects a C expression. The
--   @block@ quasi-quoter expects a list of statements, like the body of
--   a function. Just like a C function, a block has a return type, matching the
--   type of any values in any @return@ statements appearing in the block.
--
-- See also the @README.md@ file for more documentation.
--
-- === Anti-quoters
--
-- Haskell variables can be captured using anti-quoters.  @inline-c@
-- provides a basic anti-quoting mechanism extensible with user-defined
-- anti-quoters (see "Language.C.Inline.Context").  The basic
-- anti-quoter lets you capture Haskell variables, for
-- example we might say
--
-- @
-- let x = pi / 3 in ['C.exp'| double { cos($(double x)) } |]
-- @
--
-- Which would capture the Haskell variable @x@ of type @'CDouble'@.
--
-- In C expressions the @$@ character is denoted using @$$@.
--
-- === Variable capture and the typing relation
--
-- The Haskell type of the inlined expression is determined by the specified
-- C return type. The relation between the C type and the Haskell type is
-- defined in the current 'Context' -- see 'convertCType'. C pointers and
-- arrays are both converted to Haskell @'Ptr'@s, and function pointers are
-- converted to @'FunPtr'@s. Sized arrays are not supported.
--
-- Similarly, when capturing Haskell variables using anti-quoting, their
-- type is assumed to be of the Haskell type corresponding to the C type
-- provided.  For example, if we capture variable @x@ using @double x@
-- in the parameter list, the code will expect a variable @x@ of type
-- 'CDouble' in Haskell (when using 'baseCtx').
--
-- === Purity
--
-- The 'exp' and 'block' quasi-quotes denote computations in the 'IO' monad.
-- 'pure' denotes a pure value, expressed as a C expression.
--
-- === Safe and @unsafe@ calls
--
-- @unsafe@ variants of the quasi-quoters are provided in
-- "Language.C.Inline.Unsafe" to call the C code unsafely, in the sense that the
-- C code will block the RTS, but with the advantage of a faster call to the
-- foreign code. See
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1590008.4.3>.
--
-- == Examples
--
-- === Inline C expression
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import qualified "Language.C.Inline" as C
-- import qualified "Language.C.Inline.Unsafe" as CU
-- import           "Foreign.C.Types"
--
-- C.'include' "\<math.h\>"
--
-- c_cos :: 'CDouble' -> IO 'CDouble'
-- c_cos x = [C.exp| double { cos($(double x)) } |]
--
-- faster_c_cos :: 'CDouble' -> IO 'CDouble'
-- faster_c_cos x = [CU.exp| double { cos($(double x)) } |]
-- @
--
-- === Inline C statements
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import qualified Data.Vector.Storable.Mutable as V
-- import qualified "Language.C.Inline" as C
-- import           "Foreign.C.Types"
--
-- C.'include' "\<stdio.h\>"
--
-- parseVector :: 'CInt' -> 'IO' (V.IOVector 'CDouble')
-- parseVector len = do
--   vec <- V.new $ 'fromIntegral' len0
--   V.unsafeWith vec $ \\ptr -> [C.'block'| void {
--     int i;
--     for (i = 0; i < $(int len); i++) {
--       scanf("%lf ", &$(double *ptr)[i]);
--     }
--   } |]
--   'return' vec
-- @
--
-- == How it works
--
-- For each quasi-quotation of C code, a C function is generated in a C file
-- corresponding to the current Haskell file. Every inline C expression will result
-- in a corresponding C function.
-- For example, if we define @c_cos@
-- as in the example above in @CCos.hs@, we will get a file containing
--
-- @
-- #include <math.h>
--
-- double inline_c_Main_0_a03fba228a6d8e36ea7d69381f87bade594c949d(double x_inline_c_0) {
--   return cos(x_inline_c_0);
-- }
-- @
--
-- Every anti-quotation will correspond to an argument in the C function. If the same
-- Haskell variable is anti-quoted twice, this will result in two arguments.
--
-- The C function is then automatically compiled and invoked from Haskell with the correct arguments passed in.

-- | C expressions.
exp :: TH.QuasiQuoter
exp = genericQuote IO $ inlineExp TH.Safe

-- | Variant of 'exp', for use with expressions known to have no side effects.
--
-- BEWARE: use this function with caution, only when you know what you are
-- doing. If an expression does in fact have side-effects, then indiscriminate
-- use of 'pure' may endanger referential transparency, and in principle even
-- type safety.
pure :: TH.QuasiQuoter
pure = genericQuote Pure $ inlineExp TH.Safe

-- | Emits a CPP include directive for C code associated with the current
-- module. To avoid having to escape quotes, the function itself adds them when
-- appropriate, so that
--
-- @
-- include "foo.h" ==> #include "foo.h"
-- @
--
-- but
--
-- @
-- include "\<foo\>" ==> #include \<foo\>
-- @
include :: String -> TH.DecsQ
include s
  | null s = fail "inline-c: empty string (include)"
  | head s == '<' = verbatim $ "#include " ++ s
  | otherwise = verbatim $ "#include \"" ++ s ++ "\""

-- | Emits an arbitrary C string to the C code associated with the
-- current module.  Use with care.
verbatim :: String -> TH.DecsQ
verbatim s = do
  void $ emitVerbatim s
  return []

------------------------------------------------------------------------
-- setContext alias

-- | Sets the 'Context' for the current module.  This function, if
-- called, must be called before any of the other TH functions in this
-- module.  Fails if that's not the case.
context :: Context -> TH.DecsQ
context ctx = do
  setContext ctx
  return []
