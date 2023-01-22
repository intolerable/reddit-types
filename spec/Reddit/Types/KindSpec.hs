module Reddit.Types.KindSpec where

import Reddit.Types.Kind
import SpecHelpers

import Test.Hspec
import Data.Text (Text)
import Text.Megaparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Kind" do

    testHasGenericInstance (Proxy :: Proxy Kind)

    describe "parseKind" do
      it "parses kinds from their reddit representation" do
        "Listing" `shouldParseAs` ListingKind
        "more" `shouldParseAs` MoreKind
        "t1" `shouldParseAs` CommentKind
        "t2" `shouldParseAs` AccountKind
        "t3" `shouldParseAs` LinkKind
        "t4" `shouldParseAs` MessageKind
        "t5" `shouldParseAs` SubredditKind
        "t6" `shouldParseAs` AwardKind

shouldParseAs :: Text -> Kind -> Expectation
shouldParseAs t k =
  case parse parseKind "parseKind" t of
    Left err -> expectationFailure $ errorBundlePretty err
    Right res ->
      res `shouldBe` k
