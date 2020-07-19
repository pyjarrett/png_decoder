{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Word as W

import System.Environment

data PngChunk = PngSignature
    | IHDR
    | IDAT
    | IEND
    deriving (Show)

data PngReadState = PngReadState [W.Word8] [PngChunk] deriving(Show)

isPng :: [W.Word8] -> Bool
isPng (137:80:78:71:13:10:26:10:rest) = True
isPng _ = False

getChunks :: PngReadState -> [PngChunk]
getChunks (PngReadState bytes chunks) = chunks

processPng :: FilePath -> IO ()
processPng fileName = do
    contents <- B.readFile fileName
    let bytes = B.unpack contents
    let chunks = processPng' (PngReadState bytes [])
    let readState = PngReadState bytes []
    let pngChunks = getChunks (processPng' readState)
    putStrLn (show pngChunks)
    putStrLn ("Processed " ++ show (length pngChunks)  ++ " chunks")

-- decodeInt :: B.ByteString -> Int
-- decodeInt = Binary.decode

processPng' :: PngReadState -> PngReadState
processPng' (PngReadState (137:80:78:71:13:10:26:10:rest) chunks) = processPng' (PngReadState rest (PngSignature:chunks))
processPng' (PngReadState (b1:b2:b3:b4:73:72:68:82:rest) chunks) = processPng' (PngReadState rest (IHDR:chunks))
processPng' (PngReadState (b1:b2:b3:b4:73:68:65:84:rest) chunks) = processPng' (PngReadState rest (IDAT:chunks))
processPng' (PngReadState [] chunks) = PngReadState [] chunks
processPng' (PngReadState bytes chunks) = PngReadState bytes chunks

-- chunkifyPng :: [W.Word8] -> [PngChunk]
-- chunkifyPng bytes = chunks (processPng bytes)

examinePng :: FilePath -> IO ()
examinePng fileName = do
    contents <- B.readFile fileName
    if isPng (B.unpack contents)
    then processPng fileName
    else putStrLn "Not a Png."


dumpTest :: IO ()
dumpTest = do
    processPng "/home/paul/dev/calendon/assets/sprites/test_sprite.png"


main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    
    contents <- B.readFile fileName
    --putStrLn ((B.pack . take 30) contents)
    putStrLn ""
