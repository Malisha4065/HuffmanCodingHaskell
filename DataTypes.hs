module DataTypes where

import Data.Word (Word8)

-- | Huffman Tree
data HuffmanTree = Leaf Char Int
                 | Node Int HuffmanTree HuffmanTree
                 deriving (Show, Read, Eq, Ord)

-- | Switch from Char '0'/'1' to Int 0/1 for mathematical bit packing
type Bit = Int
type BitCode = [Bit]
type CodeTable = [(Char, BitCode)]

-- | The Frequency Table is what we save in the header to reconstruct the tree
type FreqTable = [(Char, Int)]

weight :: HuffmanTree -> Int
weight (Leaf _ w) = w
weight (Node w _ _) = w