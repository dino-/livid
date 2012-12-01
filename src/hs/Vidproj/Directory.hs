module Vidproj.Directory where

import Control.Monad.Error
import Data.Function ( on )
import Data.List
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.FilePath
--import Text.JSON
import Text.Printf


data Program = Program String [String]
   deriving Show


{- Get the contents of a directory and strip out the special . and
   .. dirs from the list
-}
contentsWithoutDots :: FilePath -> IO [FilePath]
contentsWithoutDots path =
   fmap (filter (`notElem` [".", ".."])) $ getDirectoryContents path


{- Extract the program name from a Program data structure
-}
progName :: Program -> String
progName (Program name _) = name


{- Get all programs and their episodes given a root directory. Data
   is contained in a list of Program data structures
-}
getPrograms :: FilePath -> IO (Either String [Program])
getPrograms root = runErrorT $ do
   topExists <- liftIO $ doesDirectoryExist root
   unless topExists $ throwError $
      printf "Directory %s does not exist!" root

   progDirs <- liftIO $ contentsWithoutDots root
   programs <- liftIO $ mapM (getProgram root) progDirs

   return $ sortBy (compare `on` progName) programs


{- Get an individual program and its episodes. Data is contained
   in a Program data structure
-}
getProgram :: FilePath -> FilePath -> IO Program
getProgram root dir = do
   epPaths <- liftIO $ contentsWithoutDots $ root </> dir
   return $ Program dir (sort epPaths)
