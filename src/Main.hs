{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as Tio
import           Options.Applicative
import           System.FilePath

import           Converter


data CmdOptions = CmdOptions
  { columns :: [String]
  , delim   :: String
  , inFile  :: Maybe String
  , outFile :: Maybe String
  , table   :: Maybe String
  } deriving (Show)


cmdOptions :: Parser CmdOptions
cmdOptions = CmdOptions
  <$> some (argument str (metavar "COLUMNS..."))
  <*> strOption
      ( long    "delimiter"
     <> short   'd'
     <> metavar "DELIM"
     <> value   ","
     <> showDefault
     <> help    "use DELIM to delimit columns in the input" )
  <*> (optional . strOption)
      ( long    "file"
     <> short   'f'
     <> metavar "FILE"
     <> help    "read FILE instead of stdin" )
  <*> (optional . strOption)
      ( long    "output"
     <> short   'o'
     <> metavar "OUTFILE"
     <> help    "write SQLite database to OUTFILE" )
  <*> (optional . strOption)
      ( long    "table"
     <> short   't'
     <> metavar "TABLE"
     <> help    "use TABLE as the SQLite table name" )


mapCmdOptions :: CmdOptions -> IO ()
mapCmdOptions (CmdOptions cols dlim inf' outf' table') = do
    xs <- T.lines <$> input
    runWith (Options (T.pack table) (T.pack outf) (T.pack <$> cols) (T.pack dlim)) xs
  where
    input = maybe Tio.getContents Tio.readFile inf'
    outf  = fromJust $ outf'
                   <|> (((<> "-out.db") . dropExtension) <$> inf')
                   <|> Just "log-data.db"
    table = fromJust $ table' <|> (dropExtension <$> inf') <|> Just "log_data"


main :: IO ()
main = execParser opts >>= mapCmdOptions
  where
    opts = info (helper <*> cmdOptions)
      ( fullDesc
     <> progDesc "Convert a log file into a SQLite table for querying" )
