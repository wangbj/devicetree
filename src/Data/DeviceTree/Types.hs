{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.DeviceTree.Types (
    DtNode
  , FdtError (..)
  , FdtToken (..)
  , FdtBlob (..)
  , FdtProp (..)
  , FdtHeader (..)
  , FdtReservedEntry (..)
  , Fdt (..)
  , parseDtb
  , fdtGetTree
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

data FdtReservedEntry = FdtReservedEntry Word64 Word64 deriving (Show, Read, Eq, Ord, Generic, GStorable)

instance Serialize FdtReservedEntry
instance NFData FdtReservedEntry

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

data FdtError = FdtErrorBadMagicNum
              | FdtErrorSizeInvalid
              | FdtErrorBadVersion
              | FdtErrorSerializeFailed String
              deriving (Show, Eq, Ord, Generic)

instance Exception FdtError
instance NFData FdtError

data FdtBlob = FdtBlob {
    fbHeader   :: FdtHeader
  , fbRsvMap   :: [FdtReservedEntry]
  , fbDtStruct :: ByteString
  , fbDtString :: ByteString
  } deriving Generic

instance NFData FdtBlob

data Fdt = Fdt FdtBlob (Tree DtNode) deriving Generic

instance Show Fdt where
  show = show . fdtGetTree

fdtGetTree :: Fdt -> Tree DtNode
fdtGetTree (Fdt _ tr) = tr

instance NFData Fdt

type DtNode = (ByteString, [ (ByteString, ByteString) ])

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft g (Left e) = Left (g e)
mapLeft g (Right x) = Right x

parseDtb_ = do
  header <- get :: Get FdtHeader
  if fdtMagic header /= fdtMagicNum then return (Left FdtErrorBadMagicNum) else
    if fdtVersion header /= 17 || fdtLastCompVersion header /= 16 then return (Left FdtErrorBadVersion) else
      if fdtTotalSize header /= min (fdtOffDtStruct header) (fdtOffDtStrings header) + fdtSizeDtStrings header + fdtSizeDtStruct header then return (Left FdtErrorSizeInvalid) else do
        let (kMem, kMemValid) = (min (fdtOffDtStruct header) (fdtOffDtStrings header) - fdtOffMemRsvmap header) `quotRem` fromIntegral (sizeOf (undefined :: FdtReservedEntry))
        if kMemValid /= 0 then return (Left FdtErrorSizeInvalid) else do
          rsv <- replicateM (fromIntegral kMem) (get :: Get FdtReservedEntry)
          (s, t) <- if fdtOffDtStruct header < fdtOffDtStrings header
            then liftA2 (,) (getBytes (fromIntegral (fdtSizeDtStruct header))) (getBytes (fromIntegral (fdtSizeDtStrings header)))
            else liftA2 (,) (getBytes (fromIntegral (fdtSizeDtStrings header))) (getBytes (fromIntegral (fdtSizeDtStruct header)))
          return (Right (header, rsv, s, t))

parseDtbHelper dtb = either (Left . FdtErrorSerializeFailed) id (runGet parseDtb_ dtb)

-- |parse dtb into a binary blob
parseDtb dtb = parseDtbHelper dtb >>= \(h, r, d, s) -> return $! FdtBlob h r d s
