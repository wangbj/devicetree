{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.ByteString as S
import           Data.ByteString (ByteString)

import           System.Environment
import           Control.Exception
import           System.IO.Error
import           System.Exit

import           Data.Tree
import           Data.DeviceTree

prettyShowDtb dtb = do
  case decode dtb of
    Left err  -> putStrLn $! "error: " ++ (show err)
    Right fdt -> putStrLn . drawTree . fmap show . dtGetTree $ fdt

usage :: IO ()
usage = getProgName >>= \prog -> putStrLn (prog ++ " <dtb>") >> exitFailure

main :: IO ()
main = getArgs >>= \case
  [] -> usage
  [x] -> S.readFile x >>= \dtb -> prettyShowDtb dtb
  (x:xs) -> usage
