{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Data.DeviceTree.Types (
    DtNode
  , parseDt
  , FdtError (..)
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
import           Control.Applicative hiding (many, many1)
import           Control.Monad
import           Data.Bits
import           Data.Tree

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
              | FdtErrorSerializeFailed String
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
  s2 <- remaining
  return (s1 - s2, (name, propData))

getProps_ strtab = (lookAhead (getProp strtab) >>= \(k, r) -> skip k >> liftM (r:) (getProps_ strtab)) <|> return []

getProps strtab nodeName = liftM (nodeName,) (getProps_ strtab)

getChildNodesEnding strtab n props k = if k <= 0 then return [Node props []] else do
  endNode <- (lookAhead (get :: Get FdtToken))
  case endNode of
    FdtTokenEndNode   -> skip (sizeOf (undefined :: FdtToken)) >> getChildNodesEnding strtab n props (k-1)
    FdtTokenBeginNode -> liftM ((Node props []):) (getChildNodes strtab n (1+k))
    _                 -> remaining >>= \r -> fail $ "failed to parse child nodes at offset: " ++ show (n-r) ++ " bytes."

getChildNodes ::ByteString -> Int -> Int -> Get (Forest (ByteString, [(ByteString, ByteString)]) )
getChildNodes strtab n k = do
  rem1 <- remaining
  void getNops
  end <- (lookAhead (get :: Get FdtToken))
  if end == FdtTokenEnd then return [] else do
    void (expect FdtTokenBeginNode)
    name <- S.pack <$> getCStringPadded
    props <- getProps strtab name
    void getNops
    endNode <- (lookAhead (get :: Get FdtToken))
    case endNode of
      FdtTokenEndNode   -> getChildNodesEnding strtab n props k
      FdtTokenBeginNode -> getChildNodes strtab n (1+k) >>= \childs -> return $! [Node props childs]
      _                 -> remaining >>= \r -> fail $ "failed to parse child nodes at offset: " ++ show (n-r) ++ " bytes."

getRootNode :: ByteString -> Int -> Get (Tree (ByteString, [(ByteString, ByteString)]))
getRootNode strtab n = do
  getNops
  childs <- getChildNodes strtab n 1
  case childs of
    []     -> (void (expect FdtTokenEnd) >> return (Node (mempty, []) []))
    [x]    -> (void (expect FdtTokenEnd)) >> remaining >>= \r -> if r == 0 then return x else fail $! "cannot consume all dtStruct from device tree, with " ++ show r ++ " bytes left."
    (x:xs) -> fail $! "device tree have multiple roots"

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

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft g (Left e) = Left (g e)
mapLeft g (Right x) = Right x

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

parseDtHelper dtb = either (Left . FdtErrorSerializeFailed) id (runGet parseDt_ dtb)

type DtNode = (ByteString, [ (ByteString, ByteString) ])

parseDt :: ByteString -> Either FdtError (Tree DtNode)
parseDt dtb = do
  (header, rsv, dt, strtab) <- parseDtHelper dtb
  let dtSize = fromIntegral (fdtSizeDtStruct header)
  mapLeft (FdtErrorSerializeFailed) (runGet (getRootNode strtab dtSize) dt)
