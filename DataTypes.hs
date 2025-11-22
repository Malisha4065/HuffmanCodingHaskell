module DataTypes where

import Data.Word (Word8)

data HuffmanTree = Leaf Char Int
                 | Node Int HuffmanTree HuffmanTree
                 deriving (Show, Eq, Ord)

-- | A Map from Char to its Bit Length (e.g., 'A' -> 3 bits)
type BitLenTable = [(Char, Int)]

-- | A Map from Char to its actual Bits (e.g., 'A' -> [1, 0, 1])
type CodeTable = [(Char, [Int])]

weight :: HuffmanTree -> Int
weight (Leaf _ w) = w
weight (Node w _ _) = w