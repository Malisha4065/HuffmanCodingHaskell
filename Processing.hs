module Processing where

import DataTypes
import Utils
import Data.List (insert)

-- | 1. Convert frequency list into a list of Leaf nodes
makeLeaves :: [(Char, Int)] -> [HuffmanTree]
makeLeaves = map (\(c, w) -> Leaf c w)

-- | 2. Core Huffman Algorithm: Combine lowest weight trees recursively
--   until one tree remains.
buildTree :: [HuffmanTree] -> HuffmanTree
buildTree [t] = t
buildTree (t1:t2:ts) = buildTree (insert newNode ts)
  where
    newNode = Node (weight t1 + weight t2) t1 t2
buildTree [] = error "Empty content cannot be compressed"

-- | Wrapper to create tree from raw string
createHuffmanTree :: String -> HuffmanTree
createHuffmanTree body = buildTree $ sortList $ makeLeaves $ frequencyCount body

-- | 3. Generate the CodeTable by traversing the Tree recursively
convertTreeToCodes :: HuffmanTree -> CodeTable
convertTreeToCodes tree = traverse tree ""
  where
    traverse (Leaf c _) code = [(c, code)]
    traverse (Node _ left right) code =
        traverse left (code ++ "0") ++ traverse right (code ++ "1")

-- | 4. Encode: Transform the string into bit sequence using the table
encode :: CodeTable -> String -> String
encode table str = concatMap (lookUpCode table) str

lookUpCode :: CodeTable -> Char -> String
lookUpCode [] _ = ""
lookUpCode ((c, code):xs) char
    | c == char = code
    | otherwise = lookUpCode xs char

-- | 5. Decode: Transform bit sequence back to string using the Tree
decode :: HuffmanTree -> String -> String
decode tree bits = decodeRec tree tree bits
  where
    -- Reached a leaf: append char and reset to root
    decodeRec _ (Leaf c _) [] = [c]
    decodeRec root (Leaf c _) rest = c : decodeRec root root rest
    -- Traversing nodes
    decodeRec _ (Node _ _ _) [] = [] -- End of stream
    decodeRec root (Node _ left right) (b:bs)
        | b == '0'  = decodeRec root left bs
        | b == '1'  = decodeRec root right bs
        | otherwise = error "Invalid bit found"