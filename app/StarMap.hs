{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative


import Linear
import qualified Data.KdMap.Static as KD
import qualified Data.HashMap.Strict as H

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Data.JsonStream.Parser
import Control.DeepSeq
import Control.Exception (evaluate)

import qualified Data.Attoparsec.ByteString.Char8 as A

import System.IO
import System.Clock
import Formatting
import Formatting.Clock


import ED.Types



clock :: Clock
clock = Monotonic


type SystemTree = KD.KdMap Double (V3 Double) System

data Galaxy = Galaxy {
      tree :: !SystemTree
    , dict :: !(H.HashMap Text System)
                     }


data QSys = QName !Text | QPoint (V3 Double) deriving Show
data REPL = Radius Double QSys | Quit | Help deriving Show


systemToKdPoint :: V3 Double -> [Double]
systemToKdPoint (V3 x y z) = [x, y, z]


replParser :: A.Parser REPL
replParser = radiusParser <|> quitParser <|> helpParser

pointParser :: A.Parser QSys
pointParser =
  QPoint <$> (V3 <$> A.double <* A.char ','
                 <*> A.double <* A.char ','
                 <*> A.double)

nameParser :: A.Parser QSys
nameParser = QName <$> fmap decodeUtf8 A.takeByteString

qsysParser :: A.Parser QSys
qsysParser = pointParser <|> nameParser

radiusParser :: A.Parser REPL
radiusParser = A.stringCI "radius"
                   *> A.space
                   *> (Radius <$> A.double
                              <* A.space
                              <*> qsysParser)
quitParser :: A.Parser REPL
quitParser = A.stringCI "quit" *> pure Quit

helpParser :: A.Parser REPL
helpParser = A.stringCI "help" *> pure Help


cmds :: Galaxy -> REPL -> IO ()
cmds (Galaxy treee d) (Radius r p) = do
  t <- getTime clock
  let maybeP = case p of
        QPoint l -> Just l
        QName n -> H.lookup (T.toUpper n) d >>= Just . location

  case maybeP of
    Nothing -> putStrLn "incorrect name!"
    Just loc -> mapM_ (\(_, s) ->  T.putStrLn $ name s) $ KD.inRadius treee r loc

  t' <- getTime clock

  fprint (timeSpecs % "\n") t t'


cmds _ Quit = return ()
cmds _ Help = putStrLn "tl;dr" >> return ()


repl :: Galaxy -> IO ()
repl galaxy = go
  where go = do
          hFlush stdout
          putStr ">>> "
          hFlush stdout
          line <- B.pack <$> getLine
          case A.parseOnly replParser line of
            Left err -> putStrLn "syntax error"
            Right repl -> cmds galaxy repl >> go

systemsToPoints :: [System] -> [(V3 Double, System)]
systemsToPoints = map (\sys -> (location sys, sys))

systemsToDicts :: [System] -> [(T.Text, System)]
systemsToDicts = map (\sys -> (T.toUpper $ name sys, sys))


openGalaxy :: IO Galaxy
openGalaxy = do
  tStart <- getTime clock
  systems <- parseLazyByteString (arrayOf parser)
                        <$> BL.readFile "data/systems.json"
  let
    tree = KD.build systemToKdPoint (systemsToPoints systems)
    dict = H.fromList . systemsToDicts $ systems

  putStrLn "Parsing..."
  _ <- evaluate $ force systems
  tParse <- getTime clock
  fprint (timeSpecs % "\n") tStart tParse

  putStrLn "Building KD-Tree..."
  _ <- evaluate $ force tree
  tTree <- getTime clock
  fprint (timeSpecs % "\n") tParse tTree

  putStrLn "Building Hash-Map..."
  _ <- evaluate $ force dict
  tHash <- getTime clock
  fprint (timeSpecs % "\n") tTree tHash

  return $! Galaxy {
                  tree = tree
                , dict = dict
                }
  where
    readIntText t = case decimal t of
                      Right x -> fst x
    v3 :: Parser (V3 Double)
    v3 = V3 <$> "x" .: real <*> "y" .: real <*> "z" .: real
    parser :: Parser System
    parser = System <$> "name" .: safeString 100
                    <*> "coords" .: v3
                    <*> (readIntText <$> "id" .: safeString 100)
                    <* "date" .: safeString 100


main :: IO ()
main = repl =<< openGalaxy
