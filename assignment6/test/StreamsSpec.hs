module StreamsSpec where
import Streams
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "fibs" $ do
    it "works the same as trivial solution" $ do
      take 20 (streamToList (interleaveStreams nats nats)) `shouldBe` [0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9]
      take 20 (streamToList ruler) `shouldBe` [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2]
    it "seed and nat works" $ property $
      \n -> take n (streamToList nats) == take n [0..]
