 {-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad (forM_)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as Tio
import Data.Monoid
import Data.String
import Database.SQLite.Simple
import System.Environment (getArgs)

data TestField = TestField Int String String deriving (Show)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field <*> field

data Options = Options
    { opName   :: Text
    , opFields :: [Text]
    , opDelim  :: Text
    }

defaults :: Options
defaults = Options "test" ["timestamp", "thread_id", "line_no", "file", "func", "message"] ","

encloseWithLR :: Monoid a => a -> a -> a -> a
encloseWithLR left right inner = left <> inner <> right

encloseWith :: Monoid a => a -> a -> a
encloseWith x = encloseWithLR x x

enclosedBy :: Monoid a => a -> a -> a
enclosedBy = flip encloseWith

sqlColumns :: [Text] -> [Text]
sqlColumns = map $ encloseWith "\"" >>> (<> " TEXT")

limitedSplitOn :: Int -> Text -> Text -> [Text]
limitedSplitOn 0 _ x         = [x]
limitedSplitOn limit delim x = if length pieces > limit
                                   then take (limit - 1) pieces ++ [T.concat (drop limit pieces)]
                                   else pieces
    where pieces = T.splitOn delim x

runWith :: Options -> [Text] -> IO ()
runWith (Options name fields delim) xs = withConnection (fileName) $ \c -> do
    execute_ c $ fromString (T.unpack $ "CREATE TABLE IF NOT EXISTS " <> tableName <> " " <> columnDef)
    execute_ c "BEGIN TRANSACTION"
    forM_ xs $ \x -> do
        execute c (fromString (T.unpack insertQuery)) (parseLine x)
    execute_ c "COMMIT"
    where
        fileName = T.unpack $ name <> ".db"
        tableName = name `enclosedBy` "\""
        columnDef = "(id INTEGER PRIMARY KEY, " <> (T.intercalate ", " (sqlColumns fields)) <> ")"
        insertQuery = "INSERT INTO " <> tableName <> " ("
                   <> T.intercalate ", " fields
                   <> ") VALUES ("
                   <> T.intercalate ", " (replicate (length fields) "?")
                   <> ")"

        parseLine :: Text -> [Text]
        parseLine x = clean <$> limitedSplitOn numFields delim x
            where
                numFields = length fields
                clean = T.dropAround (`elem` " \"")

main :: IO ()
main = do
    (fileName:_) <- getArgs
    Tio.readFile fileName >>= (T.lines >>> runWith defaults)

