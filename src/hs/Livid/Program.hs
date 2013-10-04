{-# LANGUAGE DeriveGeneric #-}

module Livid.Program
   ( Programs (..)
   , Program (..)
   , getAllPrograms
   )
where

import Control.Monad.Error
import Data.Char ( toLower )
import Data.Either ( partitionEithers )
import Data.Function ( on )
import Data.Aeson ( FromJSON, ToJSON )
--import Data.ByteString.Lazy.Char8 hiding ( filter, notElem, map )
import Data.List
import GHC.Generics ( Generic )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.FilePath
import Text.Printf
import Text.Regex

import qualified Livid.Episode as E


data Program = Program
   { title :: String
   , episodes :: [E.Episode]
   }
   deriving (Show, Generic)

instance FromJSON Program
instance ToJSON Program


newtype Programs = Programs [Program]
   deriving (Show, Generic)

instance FromJSON Programs
instance ToJSON Programs


{- Get the contents of a directory and strip out the special . and
   .. dirs from the list
-}
contentsWithoutDots :: FilePath -> IO [FilePath]
contentsWithoutDots path =
   fmap (filter (`notElem` [".", ".."])) $ getDirectoryContents path


getAllPrograms :: [FilePath] -> [String] -> IO ([String], Programs)
getAllPrograms topDirs vidExts = do
   -- Call getPrograms on all dirs, getting a [Either String Programs]
   -- Separate the errors from the good data
   -- Combine the errors into one list of String
   (errors, llps) <- fmap partitionEithers $ mapM
      (getPrograms vidExts) topDirs

   -- Combine the program data into one [Program]
   -- Sort that list
   let ps = sortBy (compare `on` (makeSortable . title))
         $ concat llps

   -- Send it all back as a tuple
   return (errors, Programs ps)


{- Get all programs and their episodes given a root directory. Data
   is contained in a list of Program data structures
-}
getPrograms :: [String] -> FilePath -> IO (Either String [Program])
getPrograms vidExts root = runErrorT $ do
   topExists <- liftIO $ doesDirectoryExist root
   unless topExists $ throwError $
      printf "Directory %s does not exist!" root

   progDirs <- liftIO $ contentsWithoutDots root
   liftIO $ do
      allProgs <- mapM (getProgram root vidExts) progDirs
      -- Get rid of shows with no episodes at this time
      return $ filter (not . null . episodes) allProgs


{- Get an individual program and its episodes. Data is contained
   in a Program data structure
-}
getProgram :: FilePath -> [String] -> FilePath -> IO Program
getProgram root vidExts item = do
   isDir <- doesDirectoryExist $ root </> item
   if isDir
      then getProgramDir root vidExts item
      else getProgramFile root vidExts item


getProgramDir :: FilePath -> [String] -> FilePath -> IO Program
getProgramDir root vidExts dir = do
   epPaths <- liftIO $ contentsWithoutDots $ root </> dir

   let filteredEps = filter (\p -> any (p `endsWith`) vidExts) epPaths
   let episodes' = map (E.mkEpisode root dir) filteredEps
   epsWithDate <- E.fillDates episodes'

   return $ Program dir
      (sortBy (compare `on` (makeSortable . E.title)) epsWithDate)


getProgramFile :: FilePath -> [String] -> FilePath -> IO Program
getProgramFile root vidExts file = do
   let epPaths = [file]

   let filteredEps = filter (\p -> any (p `endsWith`) vidExts) epPaths
   let episodes' = map (E.mkEpisode root "") filteredEps
   epsWithDate <- E.fillDates episodes'

   return $ Program (dropExtension file)
      (sortBy (compare `on` (makeSortable . E.title)) epsWithDate)


makeSortable :: String -> String
makeSortable s = foldl (flip id) s modifiers where
   modifiers =
      [ map toLower
      , \s' -> subRegex (mkRegex "^the[ ._-]") s' ""
      ]


endsWith :: Eq a => [a] -> [a] -> Bool
long `endsWith` short = (reverse short) `isPrefixOf` (reverse long)
