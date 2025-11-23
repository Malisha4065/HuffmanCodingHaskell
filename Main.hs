module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import DataTypes
import Processing
import IOHandler
import Utils

-- | Entry point: Parses command line arguments
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-c", inFile, outFile] -> runCompression inFile outFile
        ["-d", inFile, outFile] -> runDecompression inFile outFile
        ["-h"]                  -> printUsage
        _                       -> printUsage

-- | Prints help message if arguments are wrong
printUsage :: IO ()
printUsage = do
    putStrLn "Usage:"
    putStrLn "  Compress:   ./compressor -c <input_file> <output_file>"
    putStrLn "  Decompress: ./compressor -d <input_file> <output_file>"
    putStrLn "Examples:"
    putStrLn "  ./compressor -c song.txt song.huff"
    putStrLn "  ./compressor -d song.huff restored.txt"
    exitFailure

-- | Compression Logic
runCompression :: FilePath -> FilePath -> IO ()
runCompression inFile outFile = do
    putStrLn $ "Reading " ++ inFile ++ "..."
    content <- readFileContent inFile
    
    putStrLn "Analyzing frequencies..."
    let freqs = frequencyCount content
    
    putStrLn "Building Canonical Huffman Tree..."
    let rawTree = createTree freqs
    let lenTable = getBitLengths rawTree
    let canonTable = canonicalCodes lenTable
    
    putStrLn "Encoding and Bit-Packing..."
    let (packedBytes, validBits) = encode canonTable content
    
    writeBinary outFile lenTable packedBytes validBits
    
    putStrLn "Compression Complete."

-- | Decompression Logic
runDecompression :: FilePath -> FilePath -> IO ()
runDecompression inFile outFile = do
    putStrLn $ "Reading " ++ inFile ++ "..."
    (lenTable, packedBytes, validBits) <- readBinary inFile
    
    putStrLn "Reconstructing Tree from Canonical Lengths..."
    let canonTable = canonicalCodes lenTable
    let tree = rebuildTreeFromCodes canonTable
    
    putStrLn "Decoding..."
    let decoded = decode tree packedBytes validBits
    
    putStrLn $ "Writing to " ++ outFile ++ "..."
    writeFile outFile decoded
    putStrLn "Decompression Complete."