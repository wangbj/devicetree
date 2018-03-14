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
import           Criterion
import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types
import           Control.Monad

extractAll :: [EntrySelector] -> ZipArchive [(String, ByteString)]
extractAll = mapM extract
  where
    extract :: EntrySelector -> ZipArchive (String, ByteString)
    extract e = getEntry e >>= \s -> return (Text.unpack (getEntryName e), s)

getZipEntries dtbs = withArchive dtbs (extractAll =<< (Map.keys <$> getEntries))

benchDtbParsing entries = bgroup "parsing all dtbs from dtbs.zip" $
  map (\(i, s) -> bench ("parse " ++ i) $ whnf decode s) entries

-- parse all entries, this can be lazy, hence needs `nf` to bench
benchL = map (decode . snd)

benchDtbParsing2 entries = bgroup "parsing all dtbs from dtbs.zip" $ pure $ bench "parsing all" $ nf benchL entries

main :: IO ()
main = do
  entries <- getZipEntries "test/dtbs.zip"
  defaultMainWith defaultConfig {resamples = 100 } [benchDtbParsing2 entries]
