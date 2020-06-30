module CalculatorSpec where
import Calculator
import Test.Hspec
import Test.QuickCheck
import qualified StackVM as VM
import Parser

spec :: Spec
spec = do
  describe "calculator" $ do
    it "evaluates expressions" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
    it "works for some parses" $ do
       ((parseExp lit add mul "(3 * -4) + 5")  :: Maybe Integer) `shouldBe` (Just (-7))
       ((parseExp lit add mul "(3 * -4) + 5")  :: Maybe Bool) `shouldBe` (Just True)
       ((parseExp lit add mul "(3 * -4) + 5")  :: Maybe VM.Program) `shouldBe` (
         Just [
           (VM.PushI (3)),
           (VM.PushI (-4)),
           (VM.Mul),
           (VM.PushI 5),
           (VM.Add)
         ])
