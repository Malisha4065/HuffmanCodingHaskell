module IOHandler where

import System.IO
import DataTypes

-- | Reads the content of a file
readFileContent :: FilePath -> IO String
readFileContent = readFile

-- | Writes the compressed data (Code Table + Encoded Body) to a file.
-- store the Tree structure as a string
-- header so we can reconstruct it for decoding.
writeCompressed :: FilePath -> HuffmanTree -> String -> IO ()
writeCompressed path tree encodedBody = do
    let serializedTree = show tree
    -- Separator "###" helps us split tree and body during read
    writeFile path (serializedTree ++ "###" ++ encodedBody)
    putStrLn $ "Compressed data written to " ++ path

-- | Reads compressed file and splits it into Tree and Body
readCompressed :: FilePath -> IO (HuffmanTree, String)
readCompressed path = do
    content <- readFile path
    let (treeStr, body) = breakOnSeparator "###" content
    return (read treeStr :: HuffmanTree, body)

-- | Helper to split string on our custom separator
breakOnSeparator :: String -> String -> (String, String)
breakOnSeparator sep str = 
    let len = length sep
        go [] = (str, "")
        go s | take len s == sep = ("", drop len s)
             | otherwise = let (pre, post) = go (tail s) in (head s : pre, post)
    in go str