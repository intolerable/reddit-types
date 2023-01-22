module Reddit.Types.UserSpec where

import Reddit.Fixtures
import Reddit.Types.User
import Reddit.Types.Thing
import Reddit.Types.Kind
import SpecHelpers

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "User" do

    testHasGenericInstance (Proxy :: Proxy User)

    describe "/user/spez/about" do

      it "parses a user about response as a User" do
        parsedUser :: User <- loadFixture "/authenticated/user/spez/about.yaml"
        fullName parsedUser `shouldBe` FullName "1w72"
        kindOf' parsedUser `shouldBe` AccountKind

      -- it "fails to parse a user about response as a Me" do
      --   (loadFixture @(UserF '[Me]) "/authenticated/user/spez/about.yaml") `shouldThrow`
      --     \(_ :: ParseException) -> True

    describe "/user/me/about" do

      it "parses an about me response as a User" do
        _parsedUser :: User <- loadFixture "/authenticated/user/me/about.yaml"
        pure ()

      -- it "parses an about me response as a Me" do
      --   parsedMe :: User '[Me] <- loadFixture "/authenticated/user/me/about.yaml"
      --   kindOf' parsedMe `shouldBe` AccountKind
      --   fullName parsedMe `shouldBe` FullName "4gf25"
