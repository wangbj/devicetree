{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.DeviceTree.Types (
    DtNode
  , DtError (..)
  , FdtToken (..)
  , FdtBlob (..)
  , FdtProp (..)
  , FdtHeader (..)
  , DtReservedEntry (..)
  , DeviceTree (..)
  , parseDtb
  , dtGetTree
  , dtGetReservedBlock
  , fdtMagicNum
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import           Data.ByteString (ByteString)
import           Data.Word
import           Foreign.C
import           Foreign.Ptr
import           Data.Serialize
import           Foreign.Storable.Generic
import           GHC.Generics
import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Tree
import           Control.Exception
import           Control.DeepSeq

fdtMagicNum :: Word32
fdtMagicNum = 0xd00dfeed

-- |flatterned device tree header, all fields in big endian
data FdtHeader = FdtHeader {
    fdtMagic           :: Word32    -- ^magic number: 0xd00dfeed
  , fdtTotalSize       :: Word32
  , fdtOffDtStruct     :: Word32
  , fdtOffDtStrings    :: Word32
  , fdtOffMemRsvmap    :: Word32
  , fdtVersion         :: Word32    -- ^version, should be 17
  , fdtLastCompVersion :: Word32    -- ^compatible version, should be 16
  , fdtBootCpuidPhys   :: Word32
  , fdtSizeDtStrings   :: Word32
  , fdtSizeDtStruct    :: Word32
  } deriving (Show, Read, Eq, Ord, Generic, GStorable)

-- data are in big-endian format
instance Serialize FdtHeader
instance NFData FdtHeader

data DtReservedEntry = DtReservedEntry Word64 Word64 deriving (Show, Read, Eq, Ord, Generic, GStorable)

instance Serialize DtReservedEntry
instance NFData DtReservedEntry

data FdtProp = FdtProp Word32 Word32 deriving (Show, Read, Eq, Ord, Generic, GStorable)

instance Serialize FdtProp

data FdtToken = FdtTokenBeginNode
              | FdtTokenEndNode
              | FdtTokenProp
              | FdtTokenNop
              | FdtTokenEnd
              | FdtTokenLast_
  deriving (Show, Read, Eq, Ord)

instance Bounded FdtToken where
  minBound = FdtTokenBeginNode
  maxBound = FdtTokenEnd

instance Enum FdtToken where
  fromEnum FdtTokenBeginNode = 1
  fromEnum FdtTokenEndNode = 2
  fromEnum FdtTokenProp = 3
  fromEnum FdtTokenNop = 4
  fromEnum FdtTokenEnd = 9
  toEnum   1           = FdtTokenBeginNode
  toEnum   2           = FdtTokenEndNode
  toEnum   3           = FdtTokenProp
  toEnum   4           = FdtTokenNop
  toEnum   9           = FdtTokenEnd
  toEnum   _           = FdtTokenLast_

instance Storable FdtToken where
  sizeOf _    = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = toEnum . fromIntegral <$> peek (castPtr ptr :: Ptr Word32)
  poke ptr = poke (castPtr ptr :: Ptr Word32) . fromIntegral . fromEnum

instance Serialize FdtToken where
  get = toEnum . fromIntegral <$> getWord32be
  put = putWord32be . fromIntegral . fromEnum

data DtError =  DtErrorBadMagicNum       -- ^magic number mismtch
              | DtErrorSizeInvalid       -- ^invalid dtSize or stringSize
              | DtErrorBadVersion        -- ^bad dt version (not 17) or bad compatible version (<= 16)
              | DtErrorInternal String   -- ^internal error
              deriving (Show, Eq, Ord, Generic)

instance Exception DtError
instance NFData DtError

data FdtBlob = FdtBlob {
    fbHeader   :: FdtHeader
  , fbRsvMap   :: [DtReservedEntry]
  , fbDtStruct :: ByteString
  , fbDtString :: ByteString
  } deriving Generic

instance NFData FdtBlob

data DeviceTree = DeviceTree FdtBlob (Tree DtNode) deriving Generic

instance Show DeviceTree where
  show (DeviceTree (FdtBlob header rsvmap dt strtab) tr) = "DeviceTree " ++ case maybeModel of
    Nothing -> "(unknown model)"
    Just model -> ("(model: " ++ (C.unpack . C.takeWhile (/= '\0')) model++")")
    where
      maybeModel = lookup (C.pack "model") ( (snd . rootLabel) tr)

-- |get tree nodes from `DeviceTree`
dtGetTree :: DeviceTree -> Tree DtNode
dtGetTree (DeviceTree _ tr) = tr

-- |get reserved memory map from `DeviceTree`
dtGetReservedBlock :: DeviceTree -> [DtReservedEntry]
dtGetReservedBlock (DeviceTree (FdtBlob h r _ _) _) = r

instance NFData DeviceTree

type DtNode = (ByteString, [ (ByteString, ByteString) ])

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft g (Left e) = Left (g e)
mapLeft g (Right x) = Right x

parseDtb_ = do
  header <- get :: Get FdtHeader
  if fdtMagic header /= fdtMagicNum then return (Left DtErrorBadMagicNum) else
    if fdtVersion header /= 17 || fdtLastCompVersion header /= 16 then return (Left DtErrorBadVersion) else
      if fdtTotalSize header /= min (fdtOffDtStruct header) (fdtOffDtStrings header) + fdtSizeDtStrings header + fdtSizeDtStruct header then return (Left DtErrorSizeInvalid) else do
        let (kMem, kMemValid) = (min (fdtOffDtStruct header) (fdtOffDtStrings header) - fdtOffMemRsvmap header) `quotRem` fromIntegral (sizeOf (undefined :: DtReservedEntry))
        if kMemValid /= 0 then return (Left DtErrorSizeInvalid) else do
          rsv <- replicateM (fromIntegral kMem) (get :: Get DtReservedEntry)
          (s, t) <- if fdtOffDtStruct header < fdtOffDtStrings header
            then liftA2 (,) (getBytes (fromIntegral (fdtSizeDtStruct header))) (getBytes (fromIntegral (fdtSizeDtStrings header)))
            else liftA2 (,) (getBytes (fromIntegral (fdtSizeDtStrings header))) (getBytes (fromIntegral (fdtSizeDtStruct header)))
          return (Right (header, rsv, s, t))

parseDtbHelper dtb = either (Left . DtErrorInternal) id (runGet parseDtb_ dtb)

-- |parse dtb into a binary blob
parseDtb dtb = parseDtbHelper dtb >>= \(h, r, d, s) -> return $! FdtBlob h r d s
