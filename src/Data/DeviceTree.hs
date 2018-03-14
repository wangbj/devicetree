module Data.DeviceTree (
    DtNode
  , FdtError(..)
  , Fdt
  , encode
  , decode
  , fdtGetTree
  ) where

import Data.DeviceTree.Types
import qualified Data.DeviceTree.Serialize as Serialize

encode = Serialize.encode
decode = Serialize.decode
