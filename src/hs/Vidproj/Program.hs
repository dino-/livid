{-# LANGUAGE DeriveGeneric #-}

module Vidproj.Program where

import Control.Monad.Error
import Data.Function ( on )
import Data.Aeson ( FromJSON, ToJSON )
--import Data.ByteString.Lazy.Char8 hiding ( filter, notElem, map )
import Data.List
import GHC.Generics ( Generic )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.FilePath
import Text.Printf

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


{- Get all programs and their episodes given a root directory. Data
   is contained in a list of Program data structures
-}
getPrograms :: FilePath -> IO (Either String Programs)
getPrograms root = runErrorT $ do
   topExists <- liftIO $ doesDirectoryExist root
   unless topExists $ throwError $
      printf "Directory %s does not exist!" root

   progDirs <- liftIO $ contentsWithoutDots root
   programs <- liftIO $ mapM (getProgram root) progDirs

   return $ Programs $ sortBy (compare `on` title) programs


{- Get an individual program and its episodes. Data is contained
   in a Program data structure
-}
getProgram :: FilePath -> FilePath -> IO Program
getProgram root dir = do
   epPaths <- liftIO $ contentsWithoutDots $ root </> dir

   -- FIXME get real dates instead of this empty string
   let epsWithDate = zip epPaths $ repeat ""

   let episodes' = map (E.mkEpisode root dir) epsWithDate
   return $ Program dir (sortBy (compare `on` E.title) episodes')
