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
import           Test.Hspec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.RawString.QQ (r)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<*), (*>))
#endif

import           Language.C.Inline.HaskellIdentifier
import           Language.C.Inline.Internal
import qualified Language.C.Types as C
import           Language.C.Types.Parse (many1)

spec :: SpecWith ()
spec = do
  describe "parsing" $ do
    it "parses an int declaration" $ do
      void $ goodParse " int x; "
    it "rejects if bad braces (2)" $ do
      badParse [r| int { x |]

    it "parses an enum" $ do
      void $ goodParse [r| enum isl_dim_type type; |]

    it "parses an isl declaration (1)" $ do
      void $ goodParse [r| isl_ctx *isl_aff_get_ctx(__isl_keep isl_aff *aff); |]

    it "parses many isl declarations" $ do
      void $ goodParse [r|
        __isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_alloc(
                __isl_take isl_set *set,
                __isl_take isl_qpolynomial *qp);
        __isl_give isl_pw_multi_aff *isl_pw_multi_aff_copy(
                __isl_keep isl_pw_multi_aff *pma);
        __isl_give isl_pw_multi_aff *isl_pw_multi_aff_project_out_map(
                __isl_take isl_space *space,
                enum isl_dim_type type,
                unsigned first, unsigned n);
        |]

    it "parses function pointers" $ do
      void $ goodParse [r| int(int (*add)(int, int)); |]
    it "parses returning function pointers" $ do
      [retType] <- goodParse [r| double (*)(double); |]
      retType `shouldBe` cty "double (*)(double)"
    it "does not parse Haskell identifier in bad position" $ do
      badParse [r| double (*)(double Foo.bar); |]

    it "parses interface" $ do
      iface <- readFile "interface"
      -- iface <- readFile "isl/include/isl"
      void $ goodParse iface

  where
    islListableTypes =
      [ "isl_val"
      , "isl_id"
      , "isl_aff"
      , "isl_pw_aff"
      , "isl_pw_multi_aff"
      , "isl_union_pw_aff"
      , "isl_union_pw_multi_aff"
      , "isl_pw_qpolynomial"
      , "isl_pw_qpolynomial_fold"
      , "isl_constraint"
      , "isl_basic_set"
      , "isl_set"
      , "isl_basic_map"
      , "isl_map"
      , "isl_union_set"
      , "isl_union_map"
      , "isl_ast_expr"
      , "isl_ast_node"
      ]

    otherIslValTypes =
      [ "isl_qpolynomial"
      , "isl_qpolynomial_fold"
      , "isl_pw_qpolynomial_fold"
      , "isl_union_pw_qpolynomial"
      , "isl_space"
      , "isl_local_space"
      -- , "isl_dim_type"
      , "isl_ctx"
      , "isl_multi_val"
      , "isl_multi_aff"
      , "isl_multi_pw_aff"
      , "isl_multi_union_pw_aff"
      , "isl_id_to_ast_expr"
      , "isl_point"
      , "isl_vec"
      , "isl_mat"
      , "isl_vertices"
      , "isl_vertex"
      , "isl_cell"
      , "isl_restriction"
      , "isl_union_access_info"
      , "isl_union_info"
      , "isl_union_flow"
      , "isl_schedule"
      , "isl_schedule_constraints"
      , "isl_schedule_node"
      , "isl_ast_build"
      , "isl_ast_expr"
      , "isl_stride_info"
      , "isl_fixed_box"
      , "isl_bool"
      , "size_t"
      ]

    mkListIdent (C.CIdentifier x) = C.CIdentifier (x ++ "_list")

    islTypes = foldMap HashSet.fromList
      [ islListableTypes
      , fmap mkListIdent islListableTypes
      , otherIslValTypes
      ]

    assertParse ctxF p s =
      let p' = C.P $ lift spaces *> p <* lift eof
      in case C.runCParser (ctxF islTypes) "spec" s p' of
           Left err -> error $ "Parse error (assertParse): " ++ show err
           Right x -> x

    -- We use show + length to fully evaluate the result -- there
    -- might be exceptions hiding.  TODO get rid of exceptions.
    strictParse
      :: String
      -> IO [C.Type C.CIdentifier]
    strictParse s = do
      let retType = assertParse haskellCParserContext
            (many1 parseTypedC) s
      void $ evaluate $ length $ show retType
      return retType

    goodParse = strictParse
    badParse s = strictParse s `shouldThrow` anyException

    cty :: String -> C.Type C.CIdentifier
    cty s = C.parameterDeclarationType $
      assertParse C.cCParserContext C.parseParameterDeclaration s
