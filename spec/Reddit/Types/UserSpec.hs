module Reddit.Types.UserSpec where

import Reddit.Fixtures
import Reddit.Types.User

import Control.Monad.IO.Class
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "User" do

    describe "/user/spez/about" do

      it "parses an about page response" do
        res <- loadFixture "/user/spez/about.json"
        res `shouldBe` User
        liftIO $ print User
