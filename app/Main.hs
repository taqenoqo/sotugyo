{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Control.Monad
import Control.Exception
import Data.Either
import qualified Data.Map as Map
import Text.CSV
import qualified Data.Yaml as Yaml
import qualified Data.Text as Text
import Problem
import Coins12

type CategoryDict c = Map.Map String [c]

data ParseException = ParseException String

instance Exception ParseException

instance Show ParseException where
  show (ParseException s) = "parse exception (" ++ s ++ ")"

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ fail "Invalid arguments.\nUsage: sotugyo twins_data.csv rule.yaml"
  let creditsFile      = args !! 0
  let categoryDictFile = args !! 1
  (creditsDefinedInFile :: [Credit Category]) <- constructCreditsFromFiles creditsFile categoryDictFile
  let credits = indexCredits $ replaceCredits creditsDefinedInFile
  print credits
  solution <- proveGraduation graduationRule credits
  putStr solution
  return ()

indexCredits :: IsCategory c => [Credit c] -> [Credit c]
indexCredits credit = zipWith setId credit [0..] where
  setId credit id = credit { creditId = id }

constructCreditsFromFiles :: (Yaml.FromJSON c, IsCategory c) => FilePath -> FilePath -> IO [Credit c]
constructCreditsFromFiles creditsFile categoryDictFile = do
  credits <- readCreditsFile creditsFile
  categoryDict <- readCategoryDictFile categoryDictFile
  return $ combineCreditsWithDict credits categoryDict
  where
    combineCreditsWithDict credits categoryDict = do
      credit <- credits
      return $ case Map.lookup (creditName credit) categoryDict of
        Just item -> credit { creditCategories = item }
        Nothing   -> credit

readCategoryDictFile :: (Yaml.FromJSON c, IsCategory c) => FilePath -> IO (CategoryDict c)
readCategoryDictFile path = do
  parseResult <- Yaml.decodeFileEither path
  case parseResult of
    Left e -> throwIO e
    Right r -> return r

readCreditsFile :: (Yaml.FromJSON c, IsCategory c) => FilePath -> IO [Credit c]
readCreditsFile path = do
  csv <- parseCSVFromFile' path
  let csvBody = init $ tail csv
  let acquiredCreditRecords = filter isAcquired csvBody
  return $ constructCredits acquiredCreditRecords
  where
    parseCSVFromFile' path = do
      parseResult <- parseCSVFromFile path
      case parseResult of
        Left e    -> throwIO $ ParseException $ show e
        Right csv -> return csv

    isAcquired record = case record !! 6 of
      "A+" -> True
      "A"  -> True
      "B"  -> True
      "C"  -> True
      "P"  -> True
      _    -> False

    constructCredits csv = map constructCredit csv where
      constructCredit record =
        let name = record !! 4 in
        let num = read $ record !! 5 in
        Credit 0 name num []

