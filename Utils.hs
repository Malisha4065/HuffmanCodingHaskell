module Utils where

import Data.List (sortBy, group, sort)
import Data.Ord (comparing)
import Data.Word (Word8)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))

sortList :: Ord a => [a] -> [a]
sortList = sort

frequencyCount :: String -> [(Char, Int)]
frequencyCount str = map (\x -> (head x, length x)) (group (sort str))

sortByFreq :: [(Char, Int)] -> [(Char, Int)]
sortByFreq = sortBy (comparing snd)

-- | Convert an Int (Header Length) to 4 Bytes (Big Endian)
intToBytes :: Int -> [Word8]
intToBytes n = map (\i -> fromIntegral (shiftR n i .&. 0xFF)) [24, 16, 8, 0]

-- | Convert 4 Bytes back to an Int
bytesToInt :: [Word8] -> Int
bytesToInt [b1, b2, b3, b4] = 
    (shiftL (fromIntegral b1) 24) .|.
    (shiftL (fromIntegral b2) 16) .|.
    (shiftL (fromIntegral b3) 8)  .|.
    (fromIntegral b4)
bytesToInt _ = error "Invalid header size bytes"