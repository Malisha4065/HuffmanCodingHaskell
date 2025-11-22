module Main where

import System.IO
import DataTypes
import Processing
import IOHandler
import Utils

main :: IO ()
main = do
    putStrLn "--- Canonical Huffman Compressor ---"
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
    
    putStrLn "Building Canonical Codes..."
    let freqs = frequencyCount content
    let rawTree = createTree freqs
    let lenTable = getBitLengths rawTree      -- Get lengths (Standard -> Canonical)
    let canonTable = canonicalCodes lenTable  -- Assign codes sequentially
    
    putStrLn "Streaming Encode..."
    let (packedBytes, validBits) = encode canonTable content
    
    putStrLn "Output filename:"
    outFile <- getLine
    writeBinary outFile lenTable packedBytes validBits
    
    putStrLn "Done."

decompress :: IO ()
decompress = do
    putStrLn "File to decompress:"
    inFile <- getLine
    
    putStrLn "Reading Binary..."
    (lenTable, packedBytes, validBits) <- readBinary inFile
    
    putStrLn "Reconstructing Canonical Tree..."
    let canonTable = canonicalCodes lenTable
    let tree = rebuildTreeFromCodes canonTable
    
    putStrLn "Decoding..."
    let decoded = decode tree packedBytes validBits
    
    putStrLn "Output filename:"
    outFile <- getLine
    writeFile outFile decoded
    putStrLn "Success!"