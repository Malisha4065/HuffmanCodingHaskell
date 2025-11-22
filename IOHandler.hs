module IOHandler where

import System.IO
import DataTypes
import Utils
import Processing
import qualified Data.ByteString as B
import Data.Word (Word8)
import Data.Char (ord, chr)

-- | Write Canonical Binary Format
--   Header: [NumEntries (1B)] + [(Char (1B), Len (1B))...]
--   Padding: [ValidBitsInLastByte (1B)]
--   Body: [Compressed Bytes]
writeBinary :: FilePath -> BitLenTable -> [Word8] -> Int -> IO ()
writeBinary path lenTable bodyBytes validBits = do
    let numEntries = fromIntegral (length lenTable) :: Word8
    
    -- Serialize Table: Char (1 byte) + Length (1 byte)
    let tableBytes = concatMap (\(c, l) -> [fromIntegral (ord c), fromIntegral l]) lenTable
    
    let finalData = B.concat [
            B.singleton numEntries,
            B.pack tableBytes,
            B.singleton (fromIntegral validBits),
            B.pack bodyBytes
          ]
    
    B.writeFile path finalData
    putStrLn $ "Compressed to " ++ path

readBinary :: FilePath -> IO (BitLenTable, [Word8], Int)
readBinary path = do
    content <- B.readFile path
    
    let (numEntriesBS, rest1) = B.splitAt 1 content
    let numEntries = fromIntegral (B.head numEntriesBS)
    
    -- Read Table (2 * numEntries bytes)
    let (tableBS, rest2) = B.splitAt (numEntries * 2) rest1
    let table = parseTable (B.unpack tableBS)
    
    -- Read Padding Info
    let (padBS, bodyBS) = B.splitAt 1 rest2
    let validBits = fromIntegral (B.head padBS)
    
    return (table, B.unpack bodyBS, validBits)

  where
    parseTable [] = []
    parseTable (c:l:xs) = (chr (fromIntegral c), fromIntegral l) : parseTable xs

readFileContent :: FilePath -> IO String
readFileContent = readFile