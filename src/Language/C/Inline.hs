{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
