{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.KdMap.Static as KDT

--import qualified Text as T

import Data.Char (toUpper)
import Data.List (intersperse)
import Data.Monoid ((<>))
import Linear

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString.Char8 as B

import System.IO

data EDSystem = EDSystem
              { name     :: B.ByteString
              , location :: V3 Double
              } deriving (Show)


csvString :: Parser B.ByteString
csvString = char lim
            *> str
  where
    lim = '\''
    str = B.append <$> takeTill (== lim) <* char lim
                   <*> ((B.cons <$> char lim <*> str <?> "ALALALALA") <|> pure "")


sepParser = char ','

eDSystemParser :: Parser EDSystem
eDSystemParser = EDSystem <$>  csvString -- name
                          <*  sepParser
                          <*> (V3 <$> double <* sepParser
                                  <*> double <* sepParser
                                  <*> double)
                          <* sepParser
                          <* csvString -- other name ??
                          <* sepParser
                          <* csvString -- modified date

eDSystemListParser :: Parser [EDSystem]
eDSystemListParser = manyTill anyChar endOfLine -- discard first line
                   *> many (eDSystemParser <* endOfLine)

interact' f = go
  where go = do
          l <- getLine
          B.putStrLn . f $ B.pack l
          hFlush stdout
          go

main :: IO ()
main = do
  file <- B.readFile "data/System.csv"

  case parseOnly (eDSystemListParser <* endOfInput) file of
    Left err  -> putStrLn $ "Error while parsing Systems: " ++ err
    Right systems -> do
      let
          loc :: B.ByteString -> V3 Double
          loc x = location . head . filter ((== x) . name) $ systems
          systups (EDSystem n l) = (l, n)
          tree =  KDT.build (\(V3 x y z) -> [x, y, z]) (map systups systems)
          point =  KDT.inRadius tree 10.0 (V3 (-48.65625) (0.125) 41.6875)
          repl :: Parser (Double, B.ByteString)
          repl = (,) <$> (double <* space) <*> takeByteString
      --mapM_ print point
      --print $ KDT.size tree
      putStrLn "Loading done..."
      putStrLn "Syntax: <Radius-Lys> <System>"
      interact' $ either (\x -> "ERRR! " <> B.pack x)
                          (\(r, l) -> B.concat
                                      . intersperse (B.pack "\n")
                                      . map (snd)
                                      . KDT.inRadius tree r $ (loc . B.map toUpper $ l))
                 . parseOnly repl
