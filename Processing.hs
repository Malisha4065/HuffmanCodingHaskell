module Processing where

import DataTypes
import Utils
import Data.List (insert)

-- | 1. Convert frequency list into a list of Leaf nodes
makeLeaves :: [(Char, Int)] -> [HuffmanTree]
makeLeaves = map (\(c, w) -> Leaf c w)

-- | 2. Core Huffman Algorithm
buildTree :: [HuffmanTree] -> HuffmanTree
buildTree [t] = t
buildTree (t1:t2:ts) = buildTree (insert newNode ts)
  where
    newNode = Node (weight t1 + weight t2) t1 t2
buildTree [] = error "Empty content cannot be compressed"

-- | Wrapper to create tree from raw string
createHuffmanTree :: String -> HuffmanTree
createHuffmanTree body = 
    let leaves = makeLeaves $ frequencyCount body
        sortedLeaves = sortList leaves
    in if length sortedLeaves == 1
       then buildTree (Leaf '\0' 0 : sortedLeaves) -- Add dummy NULL leaf
       else buildTree sortedLeaves

-- | 3. Generate the CodeTable
convertTreeToCodes :: HuffmanTree -> CodeTable
convertTreeToCodes tree = traverse tree ""
  where
    traverse (Leaf c _) code = [(c, code)]
    traverse (Node _ left right) code =
        traverse left (code ++ "0") ++ traverse right (code ++ "1")

-- | 4. Encode
encode :: CodeTable -> String -> String
encode table str = concatMap (lookUpCode table) str

lookUpCode :: CodeTable -> Char -> String
lookUpCode [] _ = ""
lookUpCode ((c, code):xs) char
    | c == char = code
    | otherwise = lookUpCode xs char

-- | 5. Decode
decode :: HuffmanTree -> String -> String
decode tree bits = decodeRec tree tree bits
  where
    -- Edge case: If we reached a leaf and have no bits left, emit char and stop
    decodeRec _ (Leaf c _) [] = [c]
    
    -- Standard case: We reached a leaf, emit char, and reset to root with remaining bits
    decodeRec root (Leaf c _) rest = c : decodeRec root root rest
    
    -- Traversal: If we are at a Node, use the bit to decide direction
    decodeRec _ (Node _ _ _) [] = [] -- No bits left, stop
    decodeRec root (Node _ left right) (b:bs)
        | b == '0'  = decodeRec root left bs
        | b == '1'  = decodeRec root right bs
        | otherwise = error "Invalid bit found"