{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.C.Inline.ParseSpec (spec) where

import           Control.Exception (evaluate)
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import qualified Data.HashSet as HashSet
import qualified Test.Hspec as Hspec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.RawString.QQ (r)
import           Text.Regex.Posix ((=~))

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<*), (*>))
#endif

import           Language.C.Inline.HaskellIdentifier
import           Language.C.Inline.Internal
import qualified Language.C.Types as C

spec :: Hspec.SpecWith ()
spec = do
  Hspec.describe "parsing" $ do
    Hspec.it "rejects if bad braces (1)" $ do
      badParse [r| int x |]
    Hspec.it "rejects if bad braces (2)" $ do
      badParse [r| int { x |]
    Hspec.it "parses function pointers" $ do
      void $ goodParse [r| int(int (*add)(int, int)) { add(3, 4) } |]
    Hspec.it "parses returning function pointers" $ do
      (retType, params, cExp) <-
        goodParse [r| double (*)(double) { &cos } |]
      retType `Hspec.shouldBe` (cty "double (*)(double)")
      params `shouldMatchParameters` []
      cExp `shouldMatchBody` " &cos "
    Hspec.it "does not parse Haskell identifier in bad position" $ do
      badParse [r| double (*)(double Foo.bar) { 3.0 } |]
  where
    assertParse ctxF p s =
      case C.runCParser (ctxF HashSet.empty) "spec" s (lift spaces *> p <* lift eof) of
        Left err -> error $ "Parse error (assertParse): " ++ show err
        Right x -> x

    -- We use show + length to fully evaluate the result -- there
    -- might be exceptions hiding.  TODO get rid of exceptions.
    strictParse
      :: String
      -> IO (C.Type C.CIdentifier, [(C.CIdentifier, C.Type C.CIdentifier, ParameterType)], String)
    strictParse s = do
      let ParseTypedC retType pars body =
            assertParse haskellCParserContext parseTypedC s
      void $ evaluate $ length $ show (retType, pars, body)
      return (retType, pars, body)

    goodParse = strictParse
    badParse s = strictParse s `Hspec.shouldThrow` Hspec.anyException

    cty :: String -> C.Type C.CIdentifier
    cty s = C.parameterDeclarationType $
      assertParse C.cCParserContext C.parseParameterDeclaration s

    shouldMatchParameters
      :: [(C.CIdentifier, C.Type C.CIdentifier, ParameterType)]
      -> [(C.Type C.CIdentifier, ParameterType)]
      -> Hspec.Expectation
    shouldMatchParameters pars pars' =
      [(x, y) | (_, x, y) <- pars] `Hspec.shouldMatchList` pars'

    shouldMatchBody :: String -> String -> Hspec.Expectation
    shouldMatchBody x y = do
      let f ch' = case ch' of
            '(' -> "\\("
            ')' -> "\\)"
            ch -> [ch]
      (x =~ concatMap f y) `Hspec.shouldBe` True
