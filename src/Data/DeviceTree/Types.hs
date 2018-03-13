{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.DeviceTree.Types (
                             ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import           Data.ByteString (ByteString)
import           Data.Word
import           Debug.Trace
import           Foreign.C
import           Foreign.Ptr
import           Data.Serialize
import           Foreign.Storable.Generic
import           GHC.Generics
import           Control.Applicative hiding (many, many1)
import           Control.Monad
import           Data.Bits
-- import           Text.Parsec
import           Control.Monad.IO.Class

fdtMagicNum :: Word32 -- big endian
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

data FdtReservedEntry = FdtReservedEntry Word64 Word64 deriving (Show, Read, Eq, Ord, Generic, GStorable)

instance Serialize FdtReservedEntry

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
  peek ptr = (toEnum . fromIntegral) <$> peek (castPtr ptr :: Ptr Word32)
  poke ptr = poke (castPtr ptr :: Ptr Word32) . fromIntegral . fromEnum

instance Serialize FdtToken where
  get = (toEnum . fromIntegral) <$> getWord32be
  put = putWord32be . fromIntegral . fromEnum

data FdtError = FdtErrorBadMagicNum
              | FdtErrorSizeInvalid
              | FdtErrorBadVersion
              | FdtErrorSerializeFailed
              deriving (Show, Eq, Ord)

beginNode = C.unpack $! runPut (put FdtTokenBeginNode)
endNode   = C.unpack $! runPut (put FdtTokenEndNode)
propNode  = C.unpack $! runPut (put FdtTokenProp)
nopNode   = C.unpack $! runPut (put FdtTokenNop)
end_      = C.unpack $! runPut (put FdtTokenEnd)

getNop = lookAhead (get :: Get FdtToken) >>= \x -> if x == FdtTokenNop
  then skip (sizeOf (undefined :: FdtToken)) >> return (Just FdtTokenNop)
  else return Nothing

getNops = getNop >>= \x -> case x of
  Nothing -> return []
  Just nop -> liftM (nop:) (getNops <|> return [])

alignUp4 x = (x + 3) .&. (complement 3)

cstring strtab off = S.takeWhile (/= 0) (S.drop off strtab)

getProp strtab = do
  s1 <- remaining
  void getNops
  void (expect FdtTokenProp)
  p@(FdtProp len nameoff) <- get
  let lenAligned = (alignUp4 . fromIntegral) len
  propData <- getBytes lenAligned
  let name = cstring strtab (fromIntegral nameoff)
  traceShowM (name, propData, lenAligned)
  s2 <- remaining
  return (s1 - s2, (name, propData))

getProps strtab = (lookAhead (getProp strtab) >>= \(k, r) -> skip k >> liftM (r:) (getProps strtab)) <|> return []

getNode strtab = do
  rem1 <- remaining
  nop1 <- getNops
  void (expect FdtTokenBeginNode)
  name <- S.pack <$> getCStringPadded
  props <- getProps strtab
  rem3 <- remaining
  nodes <- getNodes strtab
  void getNops
  x <- getWord32be
  y <- getWord32be
  traceShowM ("...", rem1- rem3, x, y)
  void (expect FdtTokenEndNode)
  rem2 <- remaining
  return (rem1 - rem2, props)

getNodes strtab = (lookAhead (getNode strtab >>= \(k, d) -> skip k >> liftM (d++) (getNodes strtab))) <|> return []

getCStringPadded = do
  c <- getWord8
  if c == 0 then skip 3 >> return [] else do
    liftM (c:) (getCStringPaddedInternal 1)

getCStringPaddedInternal k = do
  c <- getWord8
  let k' = if 1 + k >= 4 then 0 else 1 + k
  if c /= 0 then liftM (c:) (getCStringPaddedInternal k') else do
    if k' == 0 then return [] else skip (4 - k' `mod` 4) >> return []

parseNode = do
  beginNode <- get :: Get FdtToken
  return ()

parseDt_ = do
  header <- get :: Get FdtHeader
  if fdtMagic header /= fdtMagicNum then return (Left FdtErrorBadMagicNum) else do
    if fdtVersion header /= 17 || fdtLastCompVersion header /= 16 then return (Left FdtErrorBadVersion) else do
      if fdtTotalSize header /= min (fdtOffDtStruct header) (fdtOffDtStrings header) + fdtSizeDtStrings header + fdtSizeDtStruct header then return (Left FdtErrorSizeInvalid) else do
        let (kMem, kMemValid) = (min (fdtOffDtStruct header) (fdtOffDtStrings header) - fdtOffMemRsvmap header) `quotRem` (fromIntegral (sizeOf (undefined :: FdtReservedEntry)))
        if kMemValid /= 0 then return (Left FdtErrorSizeInvalid) else do
          rsv <- replicateM (fromIntegral kMem) (get :: Get FdtReservedEntry)
          (s, t) <- if fdtOffDtStruct header < fdtOffDtStrings header
            then liftA2 (,) (getBytes (fromIntegral (fdtSizeDtStruct header))) (getBytes (fromIntegral (fdtSizeDtStrings header)))
            else liftA2 (,) (getBytes (fromIntegral (fdtSizeDtStrings header))) (getBytes (fromIntegral (fdtSizeDtStruct header)))
          return $! Right (header, rsv, s, t)

parseDt dtb = either (const (Left FdtErrorSerializeFailed)) id (runGet parseDt_ dtb)

ints :: ByteString -> [Int]
ints s = if S.null s then [] else case runGet getWord32be s1 of
  Left _ -> []
  Right x -> fromIntegral x : ints s2
  where (s1, s2) = S.splitAt 4 s
