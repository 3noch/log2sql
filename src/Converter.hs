{-# LANGUAGE OverloadedStrings #-}

module Converter
    ( Options(..)
    , runWith
    ) where

import Control.Applicative
import Control.Exception (bracketOnError, catch)
import Control.Category ((>>>))
import Control.Monad (forM_)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Monoid
import Data.String
import Database.SQLite.Simple
import System.IO (hPutStrLn, stderr)


data Options = Options
    { _opName    :: Text
    , _opOutFile :: Text
    , _opFields  :: [Text]
    , _opDelim   :: Text
    }

encloseWithLR :: Monoid a => a -> a -> a -> a
encloseWithLR left right inner = left <> inner <> right

encloseWith :: Monoid a => a -> a -> a
encloseWith x = encloseWithLR x x

enclosedBy :: Monoid a => a -> a -> a
enclosedBy = flip encloseWith

limitedSplitOn :: Int -> Text -> Text -> [Text]
limitedSplitOn 0 _ x         = [x]
limitedSplitOn limit delim x =
    if length pieces > limit
    then take (limit - 1) pieces ++ [T.concat (drop limit pieces)]
    else pieces
  where pieces = T.splitOn delim x

toQ :: Text -> Query
toQ = fromString . T.unpack

runWith :: Options -> [Text] -> IO ()
runWith (Options name outf fields delim) xs = withConnection fileName $ \c -> do
    execute_ c createQuery
    withTransaction c $
        forM_ (zip [1..] xs) $ \(i, x) ->
            execute c insertQuery (parseLine x) `catch` handleError i
  where
      fileName = T.unpack outf
      tableName = name `enclosedBy` "\""

      createQuery = toQ $ "CREATE TABLE IF NOT EXISTS " <> tableName <> " " <> columnDef

      columnDef = "(id INTEGER PRIMARY KEY, " <> T.intercalate ", " (sqlColumn <$> fields) <> ")"
        where sqlColumn = encloseWith "\"" >>> (<> " TEXT")

      insertQuery = toQ $ "INSERT INTO " <> tableName <> " ("
                 <> T.intercalate ", " fields
                 <> ") VALUES ("
                 <> T.intercalate ", " (replicate (length fields) "?")
                 <> ")"

      parseLine :: Text -> [Text]
      parseLine x = clean <$> limitedSplitOn (length fields) delim x
        where clean = T.dropAround (`elem` (" \"'" :: String))

      handleError :: Int -> FormatError -> IO ()
      handleError idx e = hPutStrLn stderr $ "Error on line " ++ show idx ++ ": " ++ show e
