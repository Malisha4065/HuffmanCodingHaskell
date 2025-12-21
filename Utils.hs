module Utils where

import Data.List (sortBy, group, sort, foldl', groupBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Data.Map.Strict as Map

-- | Standard frequency count
frequencyCount :: String -> [(Char, Int)]
frequencyCount str = map (\x -> (head x, length x)) (group (sort str))

-- | Split a list into chunks of approximately equal size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Merge multiple frequency tables into one
mergeFrequencies :: [[(Char, Int)]] -> [(Char, Int)]
mergeFrequencies tables = 
    let combinedMap = foldl' addToMap Map.empty (concat tables)
    in Map.toList combinedMap
  where
    addToMap m (c, count) = Map.insertWith (+) c count m

-- | Parallel frequency count - splits input and processes chunks concurrently
frequencyCountParallel :: String -> [(Char, Int)]
frequencyCountParallel str
    | length str < 10000 = frequencyCount str  -- Too small for parallelization
    | otherwise = 
        let numChunks = 4  -- Good for quad-core systems
            chunkSize = max 1 (length str `div` numChunks)
            chunks = chunksOf chunkSize str
            -- Process each chunk in parallel
            localCounts = parMap rdeepseq frequencyCount chunks
        in mergeFrequencies localCounts

sortList :: Ord a => [a] -> [a]
sortList = sort

-- | Int to 4 Bytes (Big Endian)
intToBytes :: Int -> [Word8]
intToBytes n = map (\i -> fromIntegral (shiftR n i .&. 0xFF)) [24, 16, 8, 0]

-- | 4 Bytes to Int
bytesToInt :: [Word8] -> Int
bytesToInt [b1, b2, b3, b4] = 
    (shiftL (fromIntegral b1) 24) .|.
    (shiftL (fromIntegral b2) 16) .|.
    (shiftL (fromIntegral b3) 8)  .|.
    (fromIntegral b4)
bytesToInt _ = 0