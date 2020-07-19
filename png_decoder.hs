{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Word as W

import System.Environment

type ByteLength = Int
type ByteContent = [W.Word8]

data PngChunk = PngSignature
    | IHDR ByteContent
    | IDAT ByteContent
    | IEND ByteContent
    | SRGB ByteContent
    | GAMA ByteContent -- Gamma Image Data
    | PHYS ByteContent -- Physical Pixel Dimensions
    | TRNS ByteContent -- Transparency
    | Unknown
    deriving (Show, Eq)

data PngReadState = PngReadState [W.Word8] [PngChunk] deriving(Show)

decodeInt32BE :: Integral a => a -> a -> a -> a -> Int
decodeInt32BE b1 b2 b3 b4 = fromIntegral ((toInteger b1) * 2^24 + (toInteger b2) * 2^16 + (toInteger b3) * 2^8 + (toInteger b4))

dumpPng :: FilePath -> IO ()
dumpPng filePath = do
    byteStringContents <- B.readFile filePath
    let byteContents = B.unpack byteStringContents
    putStrLn (show byteContents)

-- Writes out a description of all chunks in the PNG at a given FilePath.
--
describePng :: FilePath -> IO ()
describePng fileName = do
    byteStringContents <- B.readFile fileName
    let byteContents = B.unpack byteStringContents
    if isPngData byteContents
    then printPngChunks byteContents
    else putStrLn "Not a Png."
          
printPngChunks :: [W.Word8] -> IO ()
printPngChunks bytes = do
    let chunks = findPngChunks bytes
    putStrLn "Printing PNG chunks"
    mapM_ (putStrLn . show) chunks

findPngChunks :: [W.Word8] -> [PngChunk]
findPngChunks bytes = getChunks (processPng' (PngReadState bytes []))

-- Looks for the PNG signature identifying the data stream as being from a PNG.
--
isPngData :: [W.Word8] -> Bool
isPngData (137:80:78:71:13:10:26:10:rest) = True
isPngData _ = False

-- The read state has the chunks in reverse order due to how it builds the list
-- when reading the file.
getChunks :: PngReadState -> [PngChunk]
getChunks (PngReadState bytes chunks) = reverse chunks

processPng :: FilePath -> IO ()
processPng fileName = do
    contents <- B.readFile fileName
    let bytes = B.unpack contents
    let chunks = processPng' (PngReadState bytes [])
    let readState = PngReadState bytes []
    let pngChunks = getChunks (processPng' readState)
    putStrLn (show pngChunks)
    putStrLn ("Processed " ++ show (length pngChunks)  ++ " chunks")

processPng' :: PngReadState -> PngReadState
processPng' (PngReadState (137:80:78:71:13:10:26:10:rest) chunks) = processPng' (PngReadState rest   (PngSignature:chunks))
processPng' (PngReadState (b1:b2:b3:b4:73:72:68:82:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((IHDR (take chunkLength rest)):chunks))
    where byteLength = decodeInt32BE b1 b2 b3 b4
          chunkLength = byteLength + 4
processPng' (PngReadState (b1:b2:b3:b4:73:68:65:84:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((IDAT (take chunkLength rest)):chunks))
    where byteLength = decodeInt32BE b1 b2 b3 b4
          chunkLength = byteLength + 4
processPng' (PngReadState (b1:b2:b3:b4:115:82:71:66:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((SRGB (take chunkLength rest)):chunks))
    where byteLength = decodeInt32BE b1 b2 b3 b4
          chunkLength = byteLength + 4
processPng' (PngReadState (b1:b2:b3:b4:103:65:77:65:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((GAMA (take chunkLength rest)):chunks))
    where byteLength = decodeInt32BE b1 b2 b3 b4
          chunkLength = byteLength + 4
processPng' (PngReadState (b1:b2:b3:b4:112:72:89:115:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((PHYS (take chunkLength rest)):chunks))
    where byteLength = decodeInt32BE b1 b2 b3 b4
          chunkLength = byteLength + 4
processPng' (PngReadState (b1:b2:b3:b4:73:69:78:68:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((IEND (take chunkLength rest)):chunks))
    where byteLength = decodeInt32BE b1 b2 b3 b4
          chunkLength = byteLength + 4
processPng' (PngReadState (b1:b2:b3:b4:116:82:78:83:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((TRNS (take chunkLength rest)):chunks))
    where byteLength = decodeInt32BE b1 b2 b3 b4
          chunkLength = byteLength + 4
processPng' (PngReadState [] chunks) = PngReadState [] chunks
processPng' (PngReadState (x:xs) chunks) = processPng' (PngReadState xs ((Unknown):chunks))

printUsage :: IO ()
printUsage = do
    programName <- getProgName
    putStrLn (mconcat ["Usage: ", programName, "fileName", "\n",
        "\n",
        "Prints chunk type and data for the given file if a PNG."])

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
    then printUsage
    else describePng (head args) 
