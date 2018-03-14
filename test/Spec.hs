{-# LANGUAGE FlexibleContexts #-}
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Map (Map)
import           Data.Text (Text)
import           Codec.Archive.Zip
import           Data.DeviceTree
import           Data.Either
import           Test.Hspec
import           Control.Monad

extractAll :: [EntrySelector] -> ZipArchive [(String, ByteString)]
extractAll = mapM extract
  where
    extract :: EntrySelector -> ZipArchive (String, ByteString)
    extract e = getEntry e >>= \s -> return (Text.unpack (getEntryName e), s)

canParseAllDtbs dtbs = describe "parse all dtbs for Linux/arm64" $ do
  entries <- runIO $ withArchive dtbs (extractAll =<< (Map.keys <$> getEntries))
  it "dtbs is not empty" (null entries `shouldBe` False)
  mapM_ (\(i, s) -> it ("parse " ++ i) ( isRight (parseDt s) `shouldBe` True)) entries

main :: IO ()
main = hspec $ do
  canParseAllDtbs "test/dtbs.zip"
