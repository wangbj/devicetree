{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Data.DeviceTree.Serialize (
    encode
  , decode
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import           Data.ByteString (ByteString)
import           Data.Word
import           Data.Serialize hiding (encode, decode)
import qualified Data.Serialize as Serialize
import           Foreign.Storable
import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Tree
import           Data.Semigroup

import           Data.DeviceTree.Types

fdtTokenSize :: Int
fdtTokenSize = sizeOf (undefined :: FdtToken)

expectM a = (Just <$> expect a) <|> pure Nothing

getNop = lookAheadM (expectM FdtTokenNop)

getNops = getNop >>= \case
  Nothing -> return []
  Just nop -> fmap (nop:) (getNops <|> return [])

alignUp4 x = (x + 3) .&. complement 3

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

getProps_ strtab = (lookAhead (getProp strtab) >>= \(k, r) -> skip k >> fmap (r:) (getProps_ strtab)) <|> return []

getProps strtab nodeName = fmap (nodeName,) (getProps_ strtab)

getChildNodesEnding strtab n props k
  | k <= 0 = return [Node props []]
  | k  > 0 = lookAhead (get :: Get FdtToken) >>= \case
    FdtTokenEndNode   -> skip fdtTokenSize >> getChildNodesEnding strtab n props (k-1)
    FdtTokenBeginNode -> fmap (Node props []:) (getChildNodes strtab n (1+k))
    _                 -> remaining >>= \r -> fail $ "failed to parse child nodes at offset: " ++ show (n-r) ++ " bytes."

getChildNodes ::ByteString -> Int -> Int -> Get (Forest DtNode)
getChildNodes strtab n k = do
  void getNops
  end <- lookAhead (get :: Get FdtToken)
  if end == FdtTokenEnd then return [] else do
    void (expect FdtTokenBeginNode)
    name <- S.pack <$> getCStringPadded
    props <- getProps strtab name
    void getNops
    lookAhead (get :: Get FdtToken) >>= \case
      FdtTokenEndNode   -> getChildNodesEnding strtab n props k
      FdtTokenBeginNode -> getChildNodes strtab n (1+k) >>= \childs -> return [Node props childs]
      _                 -> remaining >>= \r -> fail $ "failed to parse child nodes at offset: " ++ show (n-r) ++ " bytes."

getRootNode :: ByteString -> Int -> Get (Tree DtNode)
getRootNode strtab n = 
  getNops >> getChildNodes strtab n 1 >>= \case
    []     -> (void (expect FdtTokenEnd) >> return (Node (mempty, []) []))
    [x]    -> void (expect FdtTokenEnd) >> remaining >>= \r -> if r == 0 then return x else fail $! "cannot consume all dtStruct from device tree, with " ++ show r ++ " bytes left."
    (x:xs) -> fail $! "device tree have multiple roots"

getCStringPadded = do
  c <- getWord8
  if c == 0 then skip 3 >> return [] else fmap (c:) (getCStringPaddedInternal 1)

getCStringPaddedInternal k = do
  c <- getWord8
  let k' = if 1 + k >= 4 then 0 else 1 + k
  if c /= 0 then fmap (c:) (getCStringPaddedInternal k') else
    if k' == 0 then return [] else skip (4 - k' `mod` 4) >> return []

parseNode = do
  beginNode <- get :: Get FdtToken
  return ()

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft g (Left e) = Left (g e)
mapLeft g (Right x) = Right x

decode :: ByteString -> Either FdtError Fdt
decode dtb = do
  FdtBlob header rsv dt strtab <- parseDtb dtb
  let dtSize = fromIntegral (fdtSizeDtStruct header)
  tr <- mapLeft FdtErrorSerializeFailed (runGet (getRootNode strtab dtSize) dt)
  return $! Fdt (FdtBlob header rsv dt strtab) tr

encode :: Fdt -> ByteString
encode (Fdt (FdtBlob header rsv dt strtab) _) = Serialize.encode header <> mconcat (map Serialize.encode rsv) <> dt <> strtab
