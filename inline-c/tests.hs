{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Language.C.Quote.C as C
import           Language.C.Inline
import           Language.C.Inline.Tests
import qualified Language.Haskell.TH as TH
import           Foreign.C.Types
import qualified Test.Hspec as Hspec
import qualified Data.Vector.Storable.Mutable as V

include "<math.h>"
include "<stdio.h>"

emitCode [C.cunit|
int francescos_mul(int x, int y) {
  return x * y;
}
|]

foreign import ccall "francescos_mul" francescos_mul :: Int -> Int -> Int

main :: IO ()
main = Hspec.hspec $ do
  tests
  Hspec.describe "TH" $ do
    Hspec.it "inlineCode" $ do
      let c_add = $(inlineCode $ Code
            TH.Unsafe                   -- Call safety
            [t| Int -> Int -> Int |]    -- Call type
            "francescos_add"            -- Call name
            -- C Code
            [C.cunit| int francescos_add(int x, int y) { int z = x + y; return z; } |])
      c_add 3 4 `Hspec.shouldBe` 7
    Hspec.it "inlineItems" $ do
       let c_add3 = $(inlineItems
             TH.Unsafe
             [t| CInt -> CInt |]
             [C.cty| int |]
             [C.cparams| int x |]
             [C.citems| return x + 3; |])
       c_add3 1 `Hspec.shouldBe` 1 + 3
    Hspec.it "inlineExp" $ do
      let x = $(inlineExp
            TH.Safe
            [t| CInt |]
            [C.cty| int |]
            []
            [C.cexp| 1 + 4 |])
      x `Hspec.shouldBe` 1 + 4
    Hspec.it "inlineCode" $ do
      francescos_mul 3 4 `Hspec.shouldBe` 12
    Hspec.it "cexp" $ do
      let x = 3
      let y = 4
      z <- [cexp| int(int x, int y){ x + y + 5 } |]
      z `Hspec.shouldBe` x + y + 5
    Hspec.it "cexp_unsafe" $ do
      let x = 2
      let y = 10
      z <- [cexp_unsafe| int(int x, int y){ 7 + x + y } |]
      z `Hspec.shouldBe` x + y + 7
    Hspec.it "cexp_pure" $ do
      let x = 2
      let y = 10
      let z = [cexp_pure| int(int x, int y){ x + 10 + y } |]
      z `Hspec.shouldBe` x + y + 10
    Hspec.it "cexp_pure_unsafe" $ do
      let x = 2
      let y = 10
      let z = [cexp_pure_unsafe| int(int x, int y){ x * 2 + y } |]
      z `Hspec.shouldBe` x * 2 + y
    Hspec.it "suffix type" $ do
      let x = 3
      let y = 4
      [cexp_pure| int { x_int + y_int } |] `Hspec.shouldBe` 7
    Hspec.it "void exp" $ do
      [cexp| void { printf("Hello\n") } |]
    Hspec.it "function pointer argument" $ do
      let ackermann m n
            | m == 0 = n + 1
            | m > 0 && n == 0 = ackermann (m - 1) 1
            | m > 0 && n > 0 = ackermann (m - 1) (ackermann m (n - 1))
            | otherwise = error "ackermann"
      ackermannPtr <- $(mkFunPtr [t| CInt -> CInt -> CInt |]) ackermann
      let x = 3
      let y = 4
      let z = [cexp_pure| int(int (*ackermannPtr)(int, int)) { ackermannPtr(x_int, y_int) } |]
      z `Hspec.shouldBe` ackermann x y
    Hspec.it "function pointer result" $ do
      c_add <- [cexp| int (*)(int, int) { &francescos_add } |]
      x <- $(peekFunPtr [t| CInt -> CInt -> IO CInt |]) c_add 1 2
      x `Hspec.shouldBe` 1 + 2
    Hspec.it "vectors" $ do
      let n = 10
      vec <- V.replicate (fromIntegral n) 3
      sum <- V.unsafeWith vec $ \ptr -> [citems| int(int *ptr) {
        int i;
        int x = 0;
        for (i = 0; i < n_int; i++) {
          x += ptr[i];
        }
        return x;
      } |]
      sum `Hspec.shouldBe` 3 * 10