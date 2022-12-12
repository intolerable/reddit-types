module Reddit.Types.UserSpec where

import Reddit.Fixtures
import Reddit.Types.User
import Reddit.Types.Thing
import Reddit.Types.Kind

import Control.Monad.IO.Class
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "User" do

    describe "/user/spez/about" do

      it "parses a user about response as a User" do
        parsedUser :: User <- loadFixture "/user/spez/about.yaml"
        fullName parsedUser `shouldBe` FullName Account "1w72"
        kindOf' parsedUser `shouldBe` Account
        liftIO $ print parsedUser

      it "fails to parse a user about response as a Me" do
        (loadFixture @Me "/user/spez/about.yaml") `shouldThrow`
          \(_ :: ParseException) -> True

    describe "/user/me/about" do

      it "parses an about me response as a User" do
        parsedUser :: User <- loadFixture "/user/me/about.yaml"
        liftIO $ print parsedUser

      it "parses an about me response as a Me" do
        parsedMe :: Me <- loadFixture "/user/me/about.yaml"
        kindOf' parsedMe `shouldBe` Account
        fullName parsedMe `shouldBe` FullName Account "4gf25"
        liftIO $ print parsedMe
