{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Word as W

import System.Environment

type ByteLength = Int

data PngChunk = PngSignature
    | IHDR ByteLength
    | IDAT ByteLength
    | IEND ByteLength
    | SRGB ByteLength
    | GAMA ByteLength -- Gamma Image Data
    | PHYS ByteLength -- Physical Pixel Dimensions
    | Unknown [W.Word8]
    deriving (Show)

data PngReadState = PngReadState [W.Word8] [PngChunk] deriving(Show)

decodeInt32BE :: Integral a => a -> a -> a -> a -> Integer
decodeInt32BE b1 b2 b3 b4 = toInteger((b1 * 2^24) + (b2 * 2^16) + (b3 * 2^8) + b4)

dumpTest :: IO ()
dumpTest = do
    describePng "/home/paul/dev/calendon/assets/sprites/test_sprite.png"

dumpPng :: IO ()
dumpPng = do
    byteStringContents <- B.readFile "/home/paul/dev/calendon/assets/sprites/test_sprite.png"
    let byteContents = B.unpack byteStringContents
    putStrLn (show byteContents)


-- Writes out a description of all chunks in the PNG at a given FilePath.
--
describePng :: FilePath -> IO ()
describePng fileName = do
    byteStringContents <- B.readFile fileName
    let byteContents = B.unpack byteStringContents
    if isPngData byteContents
    then printPngChunks (findPngChunks byteContents)
    else putStrLn "Not a Png."
          
printPngChunks :: [PngChunk] -> IO ()
printPngChunks chunks = do
    putStrLn "Printing PNG chunks"
    mapM_ (putStrLn . show) chunks

findPngChunks :: [W.Word8] -> [PngChunk]
findPngChunks bytes = getChunks (processPng' (PngReadState bytes []))

-- Looks for the PNG signature identifying the data stream as being from a PNG.
--
isPngData :: [W.Word8] -> Bool
isPngData (137:80:78:71:13:10:26:10:rest) = True
isPngData _ = False

-- decodeInt :: B.ByteString -> Int
-- decodeInt = Binary.decode

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
processPng' (PngReadState (b1:b2:b3:b4:73:72:68:82:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((IHDR byteLength):chunks))
    where byteLength = fromIntegral(decodeInt32BE b1 b2 b3 b4)
          chunkLength = byteLength + 4
processPng' (PngReadState (b1:b2:b3:b4:73:68:65:84:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((IDAT byteLength):chunks))
    where byteLength = fromIntegral(decodeInt32BE b1 b2 b3 b4)
          chunkLength = byteLength + 4
processPng' (PngReadState (b1:b2:b3:b4:115:82:71:66:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((SRGB byteLength):chunks))
    where byteLength = fromIntegral(decodeInt32BE b1 b2 b3 b4)
          chunkLength = byteLength + 4
processPng' (PngReadState (b1:b2:b3:b4:103:65:77:65:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((GAMA byteLength):chunks))
    where byteLength = fromIntegral(decodeInt32BE b1 b2 b3 b4)
          chunkLength = byteLength + 4
processPng' (PngReadState (b1:b2:b3:b4:112:72:89:115:rest) chunks) = processPng' (PngReadState (drop chunkLength rest) ((PHYS byteLength):chunks))
    where byteLength = fromIntegral(decodeInt32BE b1 b2 b3 b4)
          chunkLength = byteLength + 4

processPng' (PngReadState [] chunks) = PngReadState [] chunks
processPng' (PngReadState bytes chunks) = PngReadState bytes ((Unknown bytes):chunks)

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    contents <- B.readFile fileName
    putStrLn ""
