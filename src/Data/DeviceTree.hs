module Data.DeviceTree (
    DtNode
  , DtError(..)
  , DeviceTree
  , DtReservedEntry (..)
  , encode
  , decode
  , dtGetTree
  , dtGetReservedBlock
  ) where

import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import qualified Data.DeviceTree.Serialize as Serialize
import qualified Data.DeviceTree.Parse as Parse

import           Data.DeviceTree.Types

encode = Serialize.encode
decode = Serialize.decode
