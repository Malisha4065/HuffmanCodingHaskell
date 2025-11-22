module Main where

import System.IO
import DataTypes
import Processing
import IOHandler

main :: IO ()
main = do
    putStrLn "--- Functional File Compressor (Huffman) ---"
    putStrLn "Select mode:"
    putStrLn "1. Compress a file"
    putStrLn "2. Decompress a file"
    putStr "> "
    hFlush stdout
    choice <- getLine
    
    case choice of
        "1" -> runCompression
        "2" -> runDecompression
        _   -> putStrLn "Invalid choice."

runCompression :: IO ()
runCompression = do
    putStrLn "Enter filename to compress (e.g., input.txt):"
    inFile <- getLine
    content <- readFileContent inFile
    
    putStrLn "Processing..."
    let tree = createHuffmanTree content
    let table = convertTreeToCodes tree
    let compressed = encode table content
    
    putStrLn "Enter output filename (e.g., output.huff):"
    outFile <- getLine
    writeCompressed outFile tree compressed
    
    -- Calculate theoretical size reduction (simulated for text output)
    let originalBits = length content * 8
    let compressedBits = length compressed
    putStrLn $ "Original Size (bits): " ++ show originalBits
    putStrLn $ "Compressed Size (bits): " ++ show compressedBits
    putStrLn "Done."

runDecompression :: IO ()
runDecompression = do
    putStrLn "Enter filename to decompress (e.g., output.huff):"
    inFile <- getLine
    
    (tree, body) <- readCompressed inFile
    
    let decoded = decode tree body
    
    putStrLn "Enter output filename (e.g., restored.txt):"
    outFile <- getLine
    writeFile outFile decoded
    putStrLn "File restored successfully."