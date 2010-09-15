module Objdump where
  
import Data.Word
import qualified Data.ByteString.Lazy as L
import Data.Binary.Put

import System.IO (openBinaryTempFile, hClose, Handle, FilePath, hGetContents, hPutStr, hFlush)
import System.Process
import System.Directory
import System.Exit

import Control.Monad
import Control.Applicative hiding ((<|>))
import Control.Arrow

import Data.List
import Data.Char
import Data.Either
import Data.Function

import Text.Parsec
import Text.Parsec.String

trim = concat . init . tail . groupBy ((==) `on` isSpace) . (" " ++) . (++ " ")

-- I should probably do this more efficiently, but whatever
parser :: Parser (String, Either Word32 (Word16, Word16))
parser = 
  do spaces *> many1 hexDigit *> char ':' *> spaces
     val <- trim <$> many1 (char ' ' <|> hexDigit)
     char '\t'
     instruction <- manyTill anyChar (choice [eof, try (spaces *> char ';') *> pure ()])
     let (x, y) = break (== ' ') val
     if ' ' `elem` val
       then return (instruction, Right (read ("0x" ++ x), read ("0x" ++ tail y)))
       else return (instruction, Left (read ("0x" ++ val)))

deTab :: String -> String
deTab = map f
  where f '\t' = ' '
        f x    = x

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile f = 
  do tempFolder <- getTemporaryDirectory
     (path, h) <- openBinaryTempFile tempFolder "tmp"
     res <- f path h
     hClose h >> removeFile path
     return res
     
disassemble :: FilePath -> [String] -> [Word32] -> IO [String]
disassemble cmd args ws = withTempFile $ \path h ->
  do L.hPut h . runPut . mapM_ putWord32le $ ws
     hFlush h
     out <- readProcess cmd (args ++ ["-D", path]) ""
     let instructions = [parse parser "" line | line <- lines out]
     return . map (deTab . fst) . rights $ instructions



disassemble16 :: FilePath -> [String] -> [Word16] -> IO [(String, Either Word16 (Word16, Word16))]
disassemble16 cmd args ws = withTempFile $ \path h ->
  do L.hPut h . runPut . mapM_ putWord16le $ ws
     hFlush h
     out <- readProcess cmd (args ++ ["-D", path]) ""
     -- putStrLn out
     let instructions = [parse parser "" line | line <- lines out]
     -- mapM_ print instructions
     return . map (deTab *** either (Left . fromIntegral) Right) . rights $ instructions
