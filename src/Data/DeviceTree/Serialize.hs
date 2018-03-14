{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Data.DeviceTree.Serialize (
    encode
  , decode
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import           Data.ByteString (ByteString)
import           Data.Word
import           Foreign.C
import           Foreign.Ptr
import           Data.Serialize hiding (encode, decode)
import           Foreign.Storable.Generic
import           GHC.Generics
import           Control.Applicative hiding (many, many1)
import           Control.Monad
import           Data.Bits
import           Data.Tree
import           Control.Exception
import           Control.DeepSeq

import           Data.DeviceTree.Types

fdtTokenSize :: Int
fdtTokenSize = sizeOf (undefined :: FdtToken)

getNop = lookAhead (get :: Get FdtToken) >>= \x -> if x == FdtTokenNop
  then skip fdtTokenSize >> return (Just FdtTokenNop)
  else return Nothing

getNops = getNop >>= \x -> case x of
  Nothing -> return []
  Just nop -> liftM (nop:) (getNops <|> return [])

alignUp4 x = (x + 3) .&. (complement 3)

cstringLen s o = let o0 = o in until (\k -> s `S.index` k == 0) (+1) o - o0

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
    FdtTokenEndNode   -> skip fdtTokenSize >> getChildNodesEnding strtab n props (k-1)
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

parseDt :: ByteString -> Either FdtError (Tree DtNode)
parseDt dtb = do
  (header, rsv, dt, strtab) <- parseDtHelper dtb
  let dtSize = fromIntegral (fdtSizeDtStruct header)
  mapLeft (FdtErrorSerializeFailed) (runGet (getRootNode strtab dtSize) dt)

decode :: ByteString -> Either FdtError Fdt
decode dtb = do
  (header, rsv, dt, strtab) <- parseDtHelper dtb
  let dtSize = fromIntegral (fdtSizeDtStruct header)
  tr <- mapLeft (FdtErrorSerializeFailed) (runGet (getRootNode strtab dtSize) dt)
  return $! (Fdt (FdtBlob header rsv dt strtab) tr)

encode :: Fdt -> ByteString
encode = error "TBD"
