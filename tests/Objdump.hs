module Objdump where
  
import Data.Word
import qualified Data.ByteString.Lazy as L
import Data.Binary.Put

import System.IO (openBinaryTempFile, hClose, Handle, FilePath, hGetContents, hPutStr, hFlush)
import System.Process
import System.Directory
import System.Exit

import Control.Monad
import Control.Applicative

import Data.List
import Data.Char
import Data.Either

import Text.Parsec
import Text.Parsec.String

-- I should probably do this more efficiently, but whatever
parser :: Parser String
parser = spaces *> many1 hexDigit *> char ':' *> spaces *> many1 hexDigit *> spaces *> 
         manyTill anyChar (choice [eof, try (spaces *> char ';') *> pure ()])

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
     return . map deTab . rights $ instructions


