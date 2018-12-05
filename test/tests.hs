{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Foreign.C.Types
import qualified Language.Haskell.TH as TH
import           Prelude
import qualified Test.Hspec as Hspec
import           Text.RawString.QQ (r)

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Internal as C
import qualified Language.C.Inline.ContextSpec
import qualified Language.C.Inline.ParseSpec
import qualified Language.C.Types as C
import qualified Language.C.Types.ParseSpec

C.context (C.baseCtx <> C.fptrCtx <> C.funCtx <> C.vecCtx <> C.bsCtx)

C.include "<math.h>"
C.include "<stddef.h>"
C.include "<stdint.h>"
C.include "<stdio.h>"

C.verbatim [r|
int francescos_mul(int x, int y) {
  return x * y;
}
|]

foreign import ccall "francescos_mul" francescos_mul :: Int -> Int -> Int

main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "Language.C.Types.Parse" Language.C.Types.ParseSpec.spec
  Hspec.describe "Language.C.Inline.Context" Language.C.Inline.ContextSpec.spec
  Hspec.describe "Language.C.Inline.Parse" Language.C.Inline.ParseSpec.spec
  Hspec.describe "TH integration" $ do
    Hspec.it "inlineCode" $ do
      let c_add = $(C.inlineCode $ C.Code
            TH.Unsafe                   -- Call safety
            [t| Int -> Int -> Int |]    -- Call type
            "francescos_add"            -- Call name
            -- C Code
            [r| int francescos_add(int x, int y) { int z = x + y; return z; } |])
      c_add 3 4 `Hspec.shouldBe` 7
    Hspec.it "inlineItems" $ do
      let c_add3 = $(C.inlineItems
            TH.Unsafe
            [t| CInt -> CInt |]
            (C.quickCParser_ "int" C.parseType)
            [("x", C.quickCParser_ "int" C.parseType)]
            [r| return x + 3; |])
      c_add3 1 `Hspec.shouldBe` 1 + 3
    Hspec.it "inlineExp" $ do
      let x = $(C.inlineExp
            TH.Safe
            [t| CInt |]
            (C.quickCParser_ "int" C.parseType)
            []
            [r| 1 + 4 |])
      x `Hspec.shouldBe` 1 + 4
    Hspec.it "inlineCode" $ do
      francescos_mul 3 4 `Hspec.shouldBe` 12
    Hspec.it "void exp" $ do
      [C.exp| void { printf("Hello\n") } |]
    Hspec.it "Foreign.C.Types library types" $ do
      sz <- [C.exp| size_t { sizeof (char) } |]
      sz `Hspec.shouldBe` 1
      um <- [C.exp| uintmax_t { UINTMAX_MAX } |]
      um `Hspec.shouldBe` maxBound
