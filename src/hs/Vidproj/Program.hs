{-# LANGUAGE DeriveGeneric #-}

module Vidproj.Program
   ( Programs (..)
   , Program (..)
   , getAllPrograms
   )
where

import Control.Monad.Error
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

import qualified Vidproj.Episode as E


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


getAllPrograms :: [FilePath] -> IO ([String], Programs)
getAllPrograms topDirs = do
   -- Call getPrograms on all dirs, getting a [Either String Programs]
   -- Separate the errors from the good data
   -- Combine the errors into one list of String
   (errors, llps) <- fmap partitionEithers $ mapM getPrograms topDirs

   -- Combine the program data into one [Program]
   -- Sort that list
   let ps = sortBy (compare `on` (makeSortable . title))
         $ concat llps

   -- Send it all back as a tuple
   return (errors, Programs ps)


{- Get all programs and their episodes given a root directory. Data
   is contained in a list of Program data structures
-}
getPrograms :: FilePath -> IO (Either String [Program])
getPrograms root = runErrorT $ do
   topExists <- liftIO $ doesDirectoryExist root
   unless topExists $ throwError $
      printf "Directory %s does not exist!" root

   progDirs <- liftIO $ contentsWithoutDots root
   liftIO $ do
      allProgs <- mapM (getProgram root) progDirs
      -- Get rid of shows with no episodes at this time
      return $ filter (not . null . episodes) allProgs


{- Get an individual program and its episodes. Data is contained
   in a Program data structure
-}
getProgram :: FilePath -> FilePath -> IO Program
getProgram root dir = do
   epPaths <- liftIO $ contentsWithoutDots $ root </> dir

   -- FIXME get real dates instead of this empty string
   let epsWithDate = zip epPaths $ repeat ""

   let episodes' = map (E.mkEpisode root dir) epsWithDate
   return $ Program dir
      (sortBy (compare `on` (makeSortable . E.title)) episodes')


makeSortable :: String -> String
makeSortable s = foldl (flip id) s modifiers where
   modifiers =
      [ \s' -> subRegex (mkRegex "^[Tt]he[ ._-]") s' ""
      ]
