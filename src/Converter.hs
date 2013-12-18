{-# LANGUAGE OverloadedStrings #-}

module Converter
    ( Options(..)
    , runWith
    ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad (forM_)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Monoid
import Data.String
import Database.SQLite.Simple

data Options = Options
    { opName    :: Text
    , opOutFile :: Text
    , opFields  :: [Text]
    , opDelim   :: Text
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

withTransaction :: Connection -> IO a -> IO a
withTransaction c f = do
    execute_ c "BEGIN IMMEDIATE TRANSACTION"
    x <- f
    execute_ c "COMMIT"
    return x

toQ :: Text -> Query
toQ = fromString . T.unpack


runWith :: Options -> [Text] -> IO ()
runWith (Options name outf fields delim) xs = withConnection (fileName) $ \c -> do
    execute_ c createQuery
    withTransaction c $ do
        forM_ xs $ \x -> do
            execute c insertQuery (parseLine x)
  where
      fileName = T.unpack outf
      tableName = name `enclosedBy` "\""
 
      createQuery = toQ $ "CREATE TABLE IF NOT EXISTS " <> tableName <> " " <> columnDef

      columnDef = "(id INTEGER PRIMARY KEY, " <> (T.intercalate ", " (sqlColumns fields)) <> ")"
        where sqlColumns = map $ encloseWith "\"" >>> (<> " TEXT")

      insertQuery = toQ $ "INSERT INTO " <> tableName <> " ("
                 <> T.intercalate ", " fields
                 <> ") VALUES ("
                 <> T.intercalate ", " (replicate (length fields) "?")
                 <> ")"

      parseLine :: Text -> [Text]
      parseLine x = clean <$> limitedSplitOn numFields delim x
        where
          numFields = length fields
          clean = T.dropAround (`elem` " \"")
