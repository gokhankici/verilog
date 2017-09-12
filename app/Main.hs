{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import Language.Verilog.Parser
import qualified Turtle as T
import Filesystem.Path.CurrentOS (encodeString)
import Control.Monad
import System.Exit

data Options = Options { filename :: T.FilePath
                       }

filenameParser :: T.Parser T.FilePath
filenameParser = T.optPath "filename" 'f' "Name of the verilog file"

parser :: T.Parser Options
parser = Options <$> filenameParser 

main :: IO ()
main = do
  Options{..} <- T.options "Runs brisk benchmarks" parser
  testBool <- T.testfile filename
  when (not testBool) $ die "verilog file does not exist"

  let filenameStr = encodeString filename
  fileContents <- readFile filenameStr

  let modules = parseFile [("timescale", "")] filenameStr fileContents
  forM_ modules print

  return ()
  

