module Processing where

import DataTypes
import Utils
import Data.List (insert)
import Data.Word (Word8)
import Data.Bits (setBit, testBit, zeroBits, shiftL)
import Data.Char (ord)

-- === TREE BUILDING ===

makeLeaves :: [(Char, Int)] -> [HuffmanTree]
makeLeaves = map (\(c, w) -> Leaf c w)

buildTree :: [HuffmanTree] -> HuffmanTree
buildTree [t] = t
buildTree (t1:t2:ts) = buildTree (insert newNode ts)
  where newNode = Node (weight t1 + weight t2) t1 t2
buildTree [] = error "Empty content"

createHuffmanTree :: [(Char, Int)] -> HuffmanTree
createHuffmanTree freqs = 
    let leaves = sortList $ makeLeaves freqs
    in if length leaves == 1
       then buildTree (Leaf '\0' 0 : leaves) -- Single char fix
       else buildTree leaves

convertTreeToCodes :: HuffmanTree -> CodeTable
convertTreeToCodes tree = traverse tree []
  where
    traverse (Leaf c _) code = [(c, code)]
    traverse (Node _ left right) code =
        traverse left (code ++ [0]) ++ traverse right (code ++ [1])

-- === BIT PACKING ===

-- | Converts a stream of Bits (0s and 1s) into a stream of Bytes (Word8)
--   Returns: (PackedBytes, PaddingCount)
packBits :: [Bit] -> ([Word8], Word8)
packBits bits = (pack bits, padding)
  where
    len = length bits
    remainder = len `mod` 8
    padding = if remainder == 0 then 0 else fromIntegral (8 - remainder)
    
    -- Pad the list with 0s at the end to make it a multiple of 8
    paddedBits = bits ++ replicate (fromIntegral padding) 0

    pack [] = []
    pack xs = toByte (take 8 xs) : pack (drop 8 xs)

    -- Helper: Turn [1,0,1,0,0,0,0,0] -> Word8
    toByte :: [Bit] -> Word8
    toByte chunk = foldl (\acc (val, idx) -> 
        if val == 1 then setBit acc (7 - idx) else acc) zeroBits (zip chunk [0..7])

-- | Unpacks Bytes back into Bits, removing the padding
unpackBits :: [Word8] -> Word8 -> [Bit]
unpackBits bytes padCount = 
    let allBits = concatMap fromByte bytes
    in take (length allBits - fromIntegral padCount) allBits
  where
    fromByte :: Word8 -> [Bit]
    fromByte b = [if testBit b i then 1 else 0 | i <- [7,6..0]]

-- === ENCODING / DECODING PIPELINES ===

encode :: CodeTable -> String -> [Bit]
encode table str = concatMap lookupCode str
  where
    lookupCode c = case lookup c table of
        Just code -> code
        Nothing   -> error $ "Character not in table: " ++ show c

decode :: HuffmanTree -> [Bit] -> String
decode tree bits = decodeRec tree tree bits
  where
    decodeRec _ (Leaf c _) [] = [c]
    decodeRec root (Leaf c _) rest = c : decodeRec root root rest
    decodeRec _ (Node _ _ _) [] = []
    decodeRec root (Node _ l r) (b:bs)
        | b == 0    = decodeRec root l bs
        | b == 1    = decodeRec root r bs
        | otherwise = error "Invalid bit"