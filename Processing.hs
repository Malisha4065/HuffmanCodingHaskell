module Processing where

import DataTypes
import Utils
import Data.List (insert, sortBy)
import Data.Ord (comparing)
import Data.Bits (setBit, testBit, zeroBits, shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8)
import qualified Data.Map as Map

-- === 1. TREE BUILDING (Standard) ===

makeLeaves :: [(Char, Int)] -> [HuffmanTree]
makeLeaves = map (\(c, w) -> Leaf c w)

buildTree :: [HuffmanTree] -> HuffmanTree
buildTree [t] = t
buildTree (t1:t2:ts) = buildTree (insert newNode ts)
  where newNode = Node (weight t1 + weight t2) t1 t2
buildTree [] = error "Empty"

createTree :: [(Char, Int)] -> HuffmanTree
createTree freqs = 
    let leaves = sortList $ makeLeaves freqs
    in if length leaves == 1
       then buildTree (Leaf '\0' 0 : leaves)
       else buildTree leaves

-- === 2. CANONICAL LOGIC ===

-- | Extract just the bit lengths from the tree (Depth = Length)
getBitLengths :: HuffmanTree -> BitLenTable
getBitLengths tree = traverse tree 0
  where
    traverse (Leaf c _) depth = [(c, depth)]
    traverse (Node _ l r) depth = traverse l (depth + 1) ++ traverse r (depth + 1)

-- | Generate Codes purely from Lengths (Canonical Algorithm)
--   1. Sort by Length, then Alphabetical
--   2. Assign codes sequentially
canonicalCodes :: BitLenTable -> CodeTable
canonicalCodes lenTable = 
    let sorted = sortBy (\(c1, l1) (c2, l2) -> 
            if l1 == l2 then compare c1 c2 else compare l1 l2) lenTable
        
        assign :: [(Char, Int)] -> Int -> Int -> CodeTable
        assign [] _ _ = []
        assign ((c, len):xs) currentCode currentLen =
            let -- Shift code left if length increases (e.g. len 3->4 means code << 1)
                nextCode = currentCode `shiftL` (len - currentLen)
                bits = intToBits len nextCode
            in (c, bits) : assign xs (nextCode + 1) len

    in assign sorted 0 0

-- | Helper: Turn an Integer into a list of Bits of specific length
intToBits :: Int -> Int -> [Int]
intToBits len val = [if testBit val i then 1 else 0 | i <- reverse [0..len-1]]

-- === 3. EFFICIENT ENCODING (Direct String -> [Word8]) ===
--   Eliminates the intermediate [Bit] list to save RAM.

encode :: CodeTable -> String -> ([Word8], Int)
encode table str = 
    let codeMap = Map.fromList table -- Fast lookup
        
        -- Accumulator: (Current Byte Value, Bits Filled Count, Output Bytes)
        go [] acc count res = (reverse (if count > 0 then (acc `shiftL` (8-count)) : res else res), count)
        go (c:cs) acc count res = 
            case Map.lookup c codeMap of
                Just bits -> 
                    let (newAcc, newCount, newRes) = pushBits acc count res bits
                    in go cs newAcc newCount newRes
                Nothing -> error "Char not in table"

        pushBits acc count res [] = (acc, count, res)
        pushBits acc count res (b:bs)
            | count == 8 = pushBits 0 0 (acc : res) (b:bs) -- Flush byte
            | otherwise  = pushBits ((acc `shiftL` 1) .|. fromIntegral b) (count + 1) res bs

    in go str 0 0 []

-- === 4. DECODING (Rebuild Tree from Canonical Codes) ===

-- | We don't need the original tree. We rebuild a clean one from the canonical codes.
rebuildTreeFromCodes :: CodeTable -> HuffmanTree
rebuildTreeFromCodes table = foldl insertCode (Node 0 (Leaf ' ' 0) (Leaf ' ' 0)) table
  where
    insertCode root (c, bits) = insertRec root bits c
    
    insertRec (Leaf _ _) [] c = Leaf c 0 -- Overwrite dummy leaf
    insertRec (Node _ l r) [] c = Leaf c 0
    insertRec (Leaf _ _) (b:bs) c = insertRec (Node 0 (Leaf '\0' 0) (Leaf '\0' 0)) (b:bs) c -- Expand leaf
    insertRec (Node w l r) (0:bs) c = Node w (insertRec l bs c) r
    insertRec (Node w l r) (1:bs) c = Node w l (insertRec r bs c)
    insertRec _ _ _ = error "Bad tree state"

decode :: HuffmanTree -> [Word8] -> Int -> String
decode tree bytes validBitsLastByte = 
    let -- Convert bytes to stream of bits (lazy)
        allBits = concatMap byteToBits (init bytes) ++ take validBitsLastByte (byteToBits (last bytes))
    in decodeStream tree tree allBits
  where
    byteToBits b = [if testBit b i then 1 else 0 | i <- [7,6..0]]

    decodeStream _ (Leaf c _) [] = [c]
    decodeStream root (Leaf c _) rest = c : decodeStream root root rest
    decodeStream _ (Node _ _ _) [] = []
    decodeStream root (Node _ l r) (b:bs)
        | b == 0 = decodeStream root l bs
        | b == 1 = decodeStream root r bs