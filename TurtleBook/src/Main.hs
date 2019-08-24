{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import Data.List as L hiding (find)
import Data.Maybe

main :: IO ()
main = putStrLn "someFunc"

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
  where extension' = fromMaybe (error "oops") . extension
        priority = zipWith (,) exts [1..]
        sorter p1 p2 = let e1 = extension' p1; e2 = extension' p2
                       in lookup e1 priority `compare` lookup e2 priority
        eqFileName p1 p2
         | length f1 == length f2 = ratio > threshold
         | otherwise = f1 == f2
           where matches = sum $ map (\b -> if b then 1 else 0) $ zipWith (==) f1 f2
                 ratio = (fromIntegral . toInteger) matches / (fromIntegral . toInteger) (length f1)
                 threshold = 0.85 :: Double
                 f1 = encodeString $ dropExtension p1
                 f2 = encodeString $ dropExtension p2

copyBooks :: FilePath -> FilePath -> IO ()
copyBooks src dest = do paths <- books src
                        mapM_ (\p -> cp p (dest </> filename p)) paths



