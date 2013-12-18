{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Maybe (fromJust)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as Tio
import           Data.Version (showVersion)
import           Options.Applicative
import           System.FilePath

import           Converter
import           Paths_log2sql (version)


data CmdOptions = CmdOptions
  { _columns :: [String]
  , _delim   :: String
  , _inFile  :: Maybe String
  , _outFile :: Maybe String
  , _table   :: Maybe String
  }


cmdOptions :: Parser CmdOptions
cmdOptions = CmdOptions
  <$> some (argument str (metavar "COLUMNS..."))
  <*> strOption
      ( long    "delimiter"
     <> short   'd'
     <> metavar "DELIM"
     <> value   ","
     <> showDefault
     <> help    "Use DELIM to delimit columns in the input" )
  <*> (optional . strOption)
      ( long    "file"
     <> short   'f'
     <> metavar "FILE"
     <> help    "Read FILE instead of stdin" )
  <*> (optional . strOption)
      ( long    "output"
     <> short   'o'
     <> metavar "OUTFILE"
     <> help    "Write SQLite database to OUTFILE (default: \"FILE-out.db\" or \"log-data.db\")" )
  <*> (optional . strOption)
      ( long    "table"
     <> short   't'
     <> metavar "TABLE"
     <> help    "Use TABLE as the SQLite table name (default: FILE or \"log_data\")" )


mapCmdOptions :: CmdOptions -> IO ()
mapCmdOptions (CmdOptions cols dlim inf' outf' table') = do
    print version
    xs <- T.lines <$> input
    runWith (Options (T.pack table) (T.pack outf) (T.pack <$> cols) (T.pack dlim)) xs
  where
    input = maybe Tio.getContents Tio.readFile inf'
    outf  = fromJust $ outf'
                   <|> (((<> "-out.db") . dropExtension) <$> inf')
                   <|> Just "log-data.db"
    table = fromJust $ table' <|> (dropExtension <$> inf') <|> Just "log_data"


addVersion :: Parser (a -> a)
addVersion = infoOption ("log2sql version " ++ showVersion version)
  ( long "version"
  <> help "Show version information" )


main :: IO ()
main = execParser opts >>= mapCmdOptions
  where
    opts = info (helper <*> addVersion <*> cmdOptions)
      ( fullDesc
     <> progDesc "Convert a log file into a SQLite table for querying" )
