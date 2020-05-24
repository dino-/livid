-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- |
   Simple module for loading config files
-}
module Livid.Conf
   ( ConfMap
   , lookupReadWithDefault
   , parseToMap
   )
   where

import Data.Map as Map hiding ( map )
import Data.Maybe ( catMaybes )
import Safe ( readMay, readNote )
import Text.Printf ( printf )
import Text.Regex ( matchRegex, mkRegex )


type ConfMap = Map String String


{- |
   Parse config file data into a simple (Map String String).

   For example, this:

   >  --- file start ---
   >  foo=one
   >  # a comment
   >
   >  bar
   >  baz-blorp=2
   >  --- file end ---

   becomes:

   >  fromList [("foo","one"),("bar",""),("baz-blorp","2")]

   Comments (prefixed with #) and blank lines in the config file 
   are discarded.
-}
parseToMap :: String -> ConfMap
parseToMap entireConf =
   fromList $ map listToPair
      $ catMaybes $ map (matchRegex re) $ lines entireConf

   where
      listToPair [k, v] = (k, v)
      listToPair _      = undefined  -- Should never happen

      re = mkRegex "^([^#][^=]*)=?(.*)"


lookupReadWithDefault :: Read b => Maybe b -> String -> ConfMap -> b
lookupReadWithDefault mbDefaultVal key confMap = case mbDefaultVal of
  Just defaultVal -> maybe defaultVal id $ readMay =<< Map.lookup key confMap
  Nothing ->
    maybe (error $ printf "Can't continue because '%s' not found in config" key) id $
    readNote (printf "Can't continue because value for '%s' isn't parseable" key) <$>
    Map.lookup key confMap
