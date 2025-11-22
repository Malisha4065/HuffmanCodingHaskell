module Processing where

import DataTypes
import Utils
import Data.List (insert, sortBy, foldl')
import Data.Bits (setBit, testBit, zeroBits, shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8)
import qualified Data.Map as Map

-- ============================================================================
-- 1. TREE BUILDING & CANONICAL CODES
-- ============================================================================

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

getBitLengths :: HuffmanTree -> BitLenTable
getBitLengths tree = traverse tree 0
  where
    traverse (Leaf c _) depth = [(c, depth)]
    traverse (Node _ l r) depth = traverse l (depth + 1) ++ traverse r (depth + 1)

-- | Canonical: Returns (Value, Length)
canonicalCodes :: BitLenTable -> CodeTable
canonicalCodes lenTable = 
    let sorted = sortBy (\(c1, l1) (c2, l2) -> 
            if l1 == l2 then compare c1 c2 else compare l1 l2) lenTable
        
        assign [] _ _ = []
        assign ((c, len):xs) currentCode currentLen =
            let nextCode = currentCode `shiftL` (len - currentLen)
            in (c, (nextCode, len)) : assign xs (nextCode + 1) len

    in assign sorted 0 0

-- ============================================================================
-- 2. ENCODER (Bitwise Shifting)
-- ============================================================================

-- | Encodes directly to Word8 using an Int accumulator.
encode :: CodeTable -> String -> ([Word8], Int)
encode table str = 
    let codeMap = Map.fromList table
        
        process [] acc count res = 
            if count > 0 
            then (reverse (fromIntegral (acc `shiftL` (8 - count)) : res), count)
            else (reverse res, 0)

        process (c:cs) acc count res =
            case Map.lookup c codeMap of
                Nothing -> error "Char not in table"
                Just (val, len) -> 
                    let newAcc = (acc `shiftL` len) .|. val
                        newCount = count + len
                    in flushBytes cs newAcc newCount res

        flushBytes cs acc count res
            | count >= 8 = 
                let byteVal = fromIntegral (acc `shiftR` (count - 8)) :: Word8
                    remCount = count - 8
                    mask = (1 `shiftL` remCount) - 1
                    remAcc = acc .&. mask
                in flushBytes cs remAcc remCount (byteVal : res)
            | otherwise = process cs acc count res

    in process str 0 0 []

-- ============================================================================
-- 3. DECODER (State Machine)
-- ============================================================================

rebuildTreeFromCodes :: CodeTable -> HuffmanTree
rebuildTreeFromCodes table = foldl insertCode (Node 0 (Leaf '\0' 0) (Leaf '\0' 0)) table
  where
    insertCode root (c, (val, len)) = insertRec root len val c
    
    -- Case 1: Reached destination (depth 0). Create the real Leaf.
    insertRec _ 0 _ c = Leaf c 0 
    
    -- Case 2: Met a Dummy Leaf but need to go deeper. Expand it into a Node!
    insertRec (Leaf _ _) d bits c = insertRec (Node 0 (Leaf '\0' 0) (Leaf '\0' 0)) d bits c

    -- Case 3: Traverse Node. Decides Left (0) or Right (1) based on bit.
    insertRec (Node w l r) d bits c = 
        let bit = testBit bits (d - 1) 
        in if not bit 
           then Node w (insertRec l (d-1) bits c) r
           else Node w l (insertRec r (d-1) bits c)

decode :: HuffmanTree -> [Word8] -> Int -> String
decode fullTree bytes validBitsInput = 
    go bytes fullTree 7 
  where
    validBitsLastByte = if validBitsInput == 0 then 8 else validBitsInput

    go [] _ _ = []
    
    go [lastByte] node bitIdx
        | bitIdx < (7 - validBitsLastByte + 1) = [] -- Stop at padding
    
    go (b:bs) node bitIdx
        | bitIdx < 0 = go bs node 7 -- Next byte
        | otherwise = 
            let direction = testBit b bitIdx
                nextNode = case node of
                    Node _ l r -> if not direction then l else r
                    Leaf _ _   -> error "Logic error: Started at leaf"
            in case nextNode of
                -- Found char: Emit 'c', restart at Root, CONSUME bit (bitIdx - 1)
                Leaf c _   -> c : go (b:bs) fullTree (bitIdx - 1)
                
                -- Keep traversing
                Node _ _ _ -> go (b:bs) nextNode (bitIdx - 1)