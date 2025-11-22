module DataTypes where

-- | A Huffman Tree is either a Leaf with a character and weight,
--   or a Node with weight and two sub-trees.
data HuffmanTree = Leaf Char Int
                 | Node Int HuffmanTree HuffmanTree
                 deriving (Show, Read, Eq)

-- | Bit is represented as a Char '0' or '1'
type Bit = Char
type BitCode = [Bit]

-- | Mapping between a character and its binary code
type CodeTable = [(Char, BitCode)]

-- | Helper to get weight of a tree node
weight :: HuffmanTree -> Int
weight (Leaf _ w) = w
weight (Node w _ _) = w

-- | Comparative instance to help with sorting trees by weight
instance Ord HuffmanTree where
    compare t1 t2 = compare (weight t1) (weight t2)