module IOHandler where

import System.IO
import DataTypes
import Processing
import Utils
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8 -- For handling the Header text
import Data.Word (Word8)

-- | Writes the binary structure:
--   [HeaderLength (4)] + [Header String] + [Padding (1)] + [Body Bytes]
writeBinary :: FilePath -> [(Char, Int)] -> [Word8] -> Word8 -> IO ()
writeBinary path freqTable bodyBytes padCount = do
    -- 1. Serialize the Frequency Table to a String (e.g. "[('a',5)...]")
    let headerStr = show freqTable
    let headerBytes = C8.pack headerStr
    let headerLen = B.length headerBytes
    
    -- 2. Convert Length to 4 bytes
    let lenBytes = B.pack (intToBytes headerLen)
    
    -- 3. Combine everything
    let finalData = B.concat [ 
            lenBytes,                  -- 4 bytes
            headerBytes,               -- N bytes
            B.singleton padCount,      -- 1 byte
            B.pack bodyBytes           -- M bytes
          ]
          
    B.writeFile path finalData
    putStrLn $ "Compressed to " ++ path

-- | Reads the binary structure and splits it back out
readBinary :: FilePath -> IO ([(Char, Int)], [Word8], Word8)
readBinary path = do
    content <- B.readFile path
    
    -- 1. Extract Header Length (First 4 bytes)
    let (lenBytes, rest1) = B.splitAt 4 content
    let headerLen = bytesToInt (B.unpack lenBytes)
    
    -- 2. Extract Header String
    let (headerBytes, rest2) = B.splitAt headerLen rest1
    let freqTable = read (C8.unpack headerBytes) :: [(Char, Int)]
    
    -- 3. Extract Padding Count (Next 1 byte)
    let (padByte, bodyBytes) = B.splitAt 1 rest2
    let padCount = B.head padByte
    
    return (freqTable, B.unpack bodyBytes, padCount)

readFileContent :: FilePath -> IO String
readFileContent = readFile