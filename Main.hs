module Main where

import System.IO
import DataTypes
import Processing
import IOHandler
import Utils

main :: IO ()
main = do
    putStrLn "--- Professional Haskell Compressor (Bit-Packed) ---"
    putStrLn "1. Compress"
    putStrLn "2. Decompress"
    putStr "> "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> compress
        "2" -> decompress
        _   -> putStrLn "Invalid"

compress :: IO ()
compress = do
    putStrLn "File to compress:"
    inFile <- getLine
    content <- readFileContent inFile
    
    putStrLn "Analyzing..."
    let freqs = frequencyCount content
    let tree = createHuffmanTree freqs
    let table = convertTreeToCodes tree
    
    putStrLn "Encoding..."
    let bits = encode table content
    
    putStrLn "Bit Packing..."
    let (packedBytes, padding) = packBits bits
    
    putStrLn "Output filename:"
    outFile <- getLine
    writeBinary outFile freqs packedBytes padding
    
    putStrLn $ "Original size (bytes): " ++ show (length content)
    putStrLn $ "Compressed size (bytes): " ++ show (length packedBytes)

decompress :: IO ()
decompress = do
    putStrLn "File to decompress:"
    inFile <- getLine
    
    putStrLn "Reading Binary..."
    (freqs, packedBytes, padding) <- readBinary inFile
    
    putStrLn "Reconstructing Tree..."
    -- Canonical-style: We rebuild the EXACT tree from the frequency map
    let tree = createHuffmanTree freqs
    
    putStrLn "Unpacking Bits..."
    let bits = unpackBits packedBytes padding
    
    putStrLn "Decoding..."
    let decoded = decode tree bits
    
    putStrLn "Output filename:"
    outFile <- getLine
    writeFile outFile decoded
    putStrLn "Success!"