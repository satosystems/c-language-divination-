{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import Data.Bifunctor (second)
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Data.String.Conversions (ConvertibleStrings(convertString))
import Data.Text (splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Conduit (simpleHttp)
import System.Directory (doesFileExist)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
  ( Cursor
  , ($//)
  , (&/)
  , attribute
  , child
  , content
  , element
  , fromDocument
  )

someFunc :: IO ()
someFunc = do
  let filename = "cache.html"
  fileExists <- doesFileExist filename
  html <-
    if fileExists
      then readFile filename
      else do
        let url =
              "https://en.wikibooks.org/wiki/C_Programming/Standard_library_reference"
        html <- convertString <$> simpleHttp url
        writeFile filename html
        return html
  let doc = parseLBS $ convertString html
  let cursor = fromDocument doc
  let cursors =
        cursor $// element "table" &/ element "tbody" &/ element "tr" &/
        element "td" &/
        element "ul" &/
        element "li" &/
        element "tt" &/
        element "a"
  let dict = foldl unify [] $ map parseAnchor cursors
  let sorted = sortDict dict
  mapM_ prettyPrint sorted

parseAnchor :: Cursor -> (T.Text, T.Text)
parseAnchor cursor =
  let href = head $ attribute "href" cursor
      headerName = splitOn "/" href !! 3
      functions = head $ concatMap content $ child cursor
   in (headerName, functions)

unify :: (Eq k) => [(k, [v])] -> (k, v) -> [(k, [v])]
unify acc (header, function) =
  case lookup header acc of
    Nothing -> (header, [function]) : acc
    Just functions ->
      (header, function : functions) : filter (\item -> fst item /= header) acc

sortDict :: (Ord k, Ord v) => [(k, [v])] -> [(k, [v])]
sortDict dict =
  let sorted = sortBy (comparing fst) dict
   in map (second sort) sorted

prettyPrint :: (T.Text, [T.Text]) -> IO ()
prettyPrint (header, functions) =
  mapM_
    (\function -> TIO.putStrLn $ T.intercalate "\t" [header, function])
    functions
