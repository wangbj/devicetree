module Main where
{-# LANGUAGE FlexibleContexts #-}
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Map (Map)
import           Data.Text (Text)
import           Codec.Archive.Zip
import           Data.DeviceTree
import qualified Data.DeviceTree.Parse as DP
import           Data.Either
import           Test.Hspec
import           Control.Monad

extractAll :: [EntrySelector] -> ZipArchive [(String, ByteString)]
extractAll = traverse extract
  where
    extract :: EntrySelector -> ZipArchive (String, ByteString)
    extract e = getEntry e >>= \s -> return (Text.unpack (getEntryName e), s)

canParseAllDtbs dtbs = describe "parse all dtbs for Linux/arm64" $ do
  entries <- runIO $ withArchive dtbs (extractAll =<< (Map.keys <$> getEntries))
  it "dtbs is not empty" (null entries `shouldBe` False)
  mapM_ (\(i, s) -> it ("parse " ++ i) ( isRight (decode s) `shouldBe` True)) entries

decodeEncodeIsId dtbs = describe "`decode . encode == id` for Linux/arm64" $ do
  entries <- runIO $ withArchive dtbs (extractAll =<< (Map.keys <$> getEntries))
  mapM_ (\(i, s) -> it ("decode . encode " ++ i) ( encode <$> decode s `shouldBe` Right s )) entries

dtbZip = "test/dtbs.zip"

main :: IO ()
main = hspec $ do
  canParseAllDtbs dtbZip
  decodeEncodeIsId dtbZip
