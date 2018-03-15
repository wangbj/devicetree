{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Data.DeviceTree.Parse (
    encode
  , decode
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import           Data.ByteString (ByteString)
import           Data.Word
import qualified Data.Serialize as Serialize
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.Attoparsec.Combinator
import           Foreign.Storable
import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Tree
import           Data.Semigroup

import           Data.DeviceTree.Types

import           Prelude hiding (take)

fdtTokenSize :: Int
fdtTokenSize = sizeOf (undefined :: FdtToken)

{-# INLINE combineWord32be #-}
combineWord32be w x y z = fromIntegral w `shiftL` 24 .|. fromIntegral x `shiftL` 16 .|. fromIntegral y `shiftL` 8 .|. fromIntegral z

anyWord32be :: Parser Word32
anyWord32be = combineWord32be
              <$> anyWord8
              <*> anyWord8
              <*> anyWord8
              <*> anyWord8

word32be :: Word32 -> Parser Word32
word32be a = word8 w *> word8 x *> word8 y *> word8 z *> return a
  where
    w = fromIntegral (a `shiftR` 24)
    x = fromIntegral (a `shiftR` 16)
    y = fromIntegral (a `shiftR` 8)
    z = fromIntegral a

anyFdtToken :: Parser FdtToken
anyFdtToken = (toEnum . fromIntegral) <$> anyWord32be

fdtToken :: FdtToken -> Parser FdtToken
fdtToken token = word32be (fromIntegral (fromEnum token)) *> return token

getNop = try (fdtToken FdtTokenNop)

getNops = many getNop

alignUp4 x = (x + 3) .&. complement 3

cstringLen s o = let o0 = o in until (\k -> s `S.index` k == 0) (+1) o - o0
cstring strtab off = S.takeWhile (/= 0) (S.drop off strtab)

getProp strtab = do
  void getNops
  void (fdtToken FdtTokenProp)
  len <- anyWord32be
  nameoff <- anyWord32be
  let lenAligned = (alignUp4 . fromIntegral) len
  propData <- take lenAligned
  let name = cstring strtab (fromIntegral nameoff)
  return (name, propData)

getProps strtab nodeName = fmap (nodeName,) (many (try (getProp strtab)))

getCStringPadded = do
  c <- anyWord8
  if c == 0 then take 3 >> return [] else fmap (c:) (getCStringPaddedInternal 1)

getCStringPaddedInternal k = do
  c <- anyWord8
  let k' = if 1 + k >= 4 then 0 else 1 + k
  if c /= 0 then fmap (c:) (getCStringPaddedInternal k') else
    if k' == 0 then return [] else take (4 - k' `mod` 4) >> return []

getChildNodesEnding strtab n props k
  | k <= 0 = return [Node props []]
  | k  > 0 = lookAhead anyFdtToken >>= \case
    FdtTokenEndNode   -> take fdtTokenSize >> getChildNodesEnding strtab n props (k-1)
    FdtTokenBeginNode -> fmap (Node props []:) (getChildNodes strtab n (1+k))
    _                 -> takeByteString >>= \r -> fail $ "failed to parse child nodes, remaining: " ++ (C.unpack r)

getChildNodes ::ByteString -> Int -> Int -> Parser (Forest DtNode)
getChildNodes strtab n k = do
  void getNops
  end <- lookAhead anyFdtToken
  if end == FdtTokenEnd then return [] else do
    void (fdtToken FdtTokenBeginNode)
    name <- S.pack <$> getCStringPadded
    props <- getProps strtab name
    void getNops
    lookAhead anyFdtToken >>= \case
      FdtTokenEndNode   -> getChildNodesEnding strtab n props k
      FdtTokenBeginNode -> getChildNodes strtab n (1+k) >>= \childs -> return [Node props childs]
      _                 -> takeByteString >>= \r -> fail $ "failed to parse child nodes, remaining: " ++ (C.unpack r)

getRootNode :: ByteString -> Int -> Parser (Tree DtNode)
getRootNode strtab n = 
  getNops >> getChildNodes strtab n 1 >>= \case
    []     -> (void (fdtToken FdtTokenEnd) >> return (Node (mempty, []) []))
    [x]    -> void (fdtToken FdtTokenEnd) >> atEnd >>= \isEnd -> if isEnd then return x else takeByteString >>= \r -> fail $! "cannot consume all dtStruct from device tree, remaining: " ++ (C.unpack r)
    (x:xs) -> fail $! "device tree have multiple roots"

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft g (Left e) = Left (g e)
mapLeft g (Right x) = Right x

decode :: ByteString -> Either FdtError Fdt
decode dtb = do
  FdtBlob header rsv dt strtab <- parseDtb dtb
  let dtSize = fromIntegral (fdtSizeDtStruct header)
  case parseOnly (getRootNode strtab dtSize) dt of
    Right r   -> Right (Fdt (FdtBlob header rsv dt strtab) r)
    Left err  -> Left (FdtErrorSerializeFailed err)

encode :: Fdt -> ByteString
encode (Fdt (FdtBlob header rsv dt strtab) _) = Serialize.encode header <> mconcat (map Serialize.encode rsv) <> dt <> strtab
