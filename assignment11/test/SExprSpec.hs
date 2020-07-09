module SExprSpec where
import Test.Hspec
import Test.QuickCheck
import SExpr
import AParser


spec :: Spec
spec = do
  describe "sexpr parser works" $ do
    it "parses the examples" $ do
     runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe`
       Just (Comb [A (I "bar"),A (I "foo"),A (N 3),A (N 5),A (N 874)],"")
