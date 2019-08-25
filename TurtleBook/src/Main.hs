{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import Data.List as L hiding (find)
import Data.Text.Metrics
import Data.Either
import Data.Maybe

parser :: Parser (FilePath, FilePath)
parser = (,) <$> argPath "src" "Book source directory"
             <*> argPath "dest" "Book destination directory"

main :: IO ()
main = do
  (src, dest) <- options "A book copy utility" parser
  copyBooks src dest

matchExt :: Text -> Pattern Text
matchExt ext = star anyChar <> text ext <> ((const "") <$> eof)

exts :: [ Text ]
exts = [ ".epub"
       , ".mobi"
       , ".pdf"
       , ".djvu"
       , ".rtf"
       ];

extPat :: Pattern Text
extPat = foldr1 (<|>) (map matchExt exts)

books :: FilePath -> IO [FilePath]
books path = do
  bookPaths <- reduce (Fold (flip (:)) [] id) $ find extPat path
  return $ nubBy eqFileName $  L.sortBy sorter bookPaths
  where extension' = fromMaybe (error "should never happen") . extension
        priority = zipWith (,) exts [1..]
        sorter p1 p2 = let e1 = extension' p1; e2 = extension' p2
                       in lookup e1 priority `compare` lookup e2 priority
        eqFileName p1 p2 = damerauLevenshteinNorm f1 f2 > 0.95
           where f1 = fromRight (error "invalid path") $ toText $ dropExtension p1
                 f2 = fromRight (error "invalid path") $ toText $ dropExtension p2

copyBooks :: FilePath -> FilePath -> IO ()
copyBooks src dest = do paths <- books src
                        mapM_ (\p -> cp p (dest </> filename p)) paths



