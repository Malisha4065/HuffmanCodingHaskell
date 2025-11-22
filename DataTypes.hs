module DataTypes where

-- | Huffman Tree (Standard)
data HuffmanTree = Leaf Char Int
                 | Node Int HuffmanTree HuffmanTree
                 deriving (Show, Eq, Ord)

-- | MAPS:
-- BitLenTable: 'A' -> 3 bits (Used for canonical generation)
type BitLenTable = [(Char, Int)]

-- | CodeTable: 'A' -> (Value, Length)
--   Example: 'A' -> (5, 3)  (Binary 101, Length 3)
type CodeTable = [(Char, (Int, Int))]

weight :: HuffmanTree -> Int
weight (Leaf _ w) = w
weight (Node w _ _) = w