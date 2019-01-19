import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib as LocalHackeRank

main :: IO ()
main = hspec $ do
  describe "LocalHackeRank" $ do
    it "bla bla bla" $ do
      head [23 ..] `shouldBe` (23 :: Int)
