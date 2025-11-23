# Canonical Huffman Compressor

**Compress Files!!!**

## 📝 Problem Description
In modern computing, efficient data storage and transmission are critical. Standard text files (ASCII/UTF-8) use a fixed width of 8 bits per character, which is wasteful for characters that appear frequently (e.g., 'e', 'a', ' ').

**The Solution:** This project implements **Canonical Huffman Coding**, a lossless data compression algorithm widely used in industrial standards like **ZIP, GZIP, and JPEG**.
* **Efficiency:** We assign shorter binary codes to frequent characters and longer codes to rare ones.
* **Real-World Optimization:** Unlike basic implementations that store the entire tree structure, our compressor uses **Canonical** state (storing only bit lengths) and **Bit Packing** (writing actual binary bits, not ASCII '0's and '1's). This mimics professional compression tools, achieving significantly smaller file sizes.

---

## 🚀 Instructions to Run

### Prerequisites
* **GHC (Glasgow Haskell Compiler)** (Version 8.0 or higher)

### 1. Compilation
First, compile the project to create an executable. This improves performance for larger files.

```bash
ghc -O2 --make Main.hs -o compressor
# (On Windows, this creates compressor.exe)
````

### 2\. Usage

The tool is run from the command line using flags.

**Syntax:**

```bash
./compressor <flag> <input_file> <output_file>
```

**Flags:**

  * `-c` : Compress a file
  * `-d` : Decompress a file
  * `-h` : Show help message

**Examples:**

```bash
# Compress 'data.txt' into 'data.huff'
./compressor -c data.txt data.huff

# Decompress 'data.huff' back to 'restored.txt'
./compressor -d data.huff restored.txt
```

-----

## 💻 Sample Input/Output

### Compression Workflow

**Command:**

```bash
./compressor -c example.txt compressed.bin
```

**Output:**

```text
Reading example.txt...
Analyzing frequencies...
Building Canonical Huffman Tree...
Encoding and Bit-Packing...
Compressed to compressed.bin
Compression Complete.
```

### Decompression Workflow

**Command:**

```bash
./compressor -d compressed.bin restored.txt
```

**Output:**

```text
Reading compressed.bin...
Reconstructing Tree from Canonical Lengths...
Decoding...
Writing to restored.txt...
Decompression Complete.
```

-----

## 🧠 Functional Programming Concepts Used

This project strictly adheres to Functional Programming principles to ensure reliability and testability.

### 1\. Pure Functions & Immutability

The core logic resides in `Processing.hs` and uses no mutable state. Functions take input and produce output deterministically.

  * **Example:** `canonicalCodes` transforms a length table into code assignments without modifying external memory.
    ```haskell
    -- Processing.hs
    canonicalCodes :: BitLenTable -> CodeTable
    ```

### 2\. Recursion

We use recursion instead of loops for tree traversal and bit stream decoding. This allows us to handle potentially infinite data streams or complex tree structures safely.

  * **Example:** The `decode` function uses a recursive state machine to traverse the Huffman Tree.
    ```haskell
    -- Processing.hs
    go (b:bs) node bitIdx
        | bitIdx < 0 = go bs node 7 -- Recursive call to next byte
    ```

### 3\. Algebraic Data Types (ADTs)

We define a custom data structure to model the Huffman Tree, ensuring type safety and preventing invalid tree states.

  * **Example:**
    ```haskell
    -- DataTypes.hs
    data HuffmanTree = Leaf Char Int
                     | Node Int HuffmanTree HuffmanTree
    ```

### 4\. Higher-Order Functions

We utilize Haskell's powerful list processing functions (`map`, `sortBy`, `foldl`) to manipulate data concisely.

  * **Example:** Sorting characters by frequency and length:
    ```haskell
    -- Processing.hs
    let sorted = sortBy (\(c1, l1) (c2, l2) -> ...) lenTable
    ```

### 5\. Modular Design

The code is separated into single-responsibility modules:

  * `Main.hs`: CLI Argument Parsing and IO Orchestration.
  * `Processing.hs`: Pure compression/decompression logic.
  * `IOHandler.hs`: Binary file reading/writing.
  * `DataTypes.hs`: Type definitions.
  * `Utils.hs`: General helper functions.