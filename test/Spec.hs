import qualified Data.ByteString as S

import           Data.DeviceTree
import           Data.Either
import           Test.Hspec
import           Control.Monad.IO.Class

junoDtbShouldParse = describe "able to parse juno.dtb built from Linux" $ do
  it "parseDt juno.dtb" $ do
    liftIO (S.readFile "juno.dtb") >>= \fdt -> isRight (parseDt fdt) `shouldBe` True

main :: IO ()
main = hspec $ do
  junoDtbShouldParse
