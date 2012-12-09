{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson ( encode )
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map
-- FIXME
import Data.Maybe ( fromJust )
import Happstack.Server
import Prelude hiding ( lookup )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import System.Posix.Files ( removeLink )
import System.Process ( runCommand )
import Text.Printf
import Text.Regex ( mkRegex, splitRegex )

import Livid.Conf
import Livid.Program


defaultPort :: Int
defaultPort = 8082


defaultConfFile :: FilePath
defaultConfFile = "livid.conf"


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   confMap <- fmap parseToMap $ readFile defaultConfFile
   serverPort <- case (lookup "httpPort" confMap) of
      Nothing -> do
         putStrLn $ printf "Using default port %d" defaultPort
         putStrLn "Specify a different port like this: ad-testsrv PORT"
         return defaultPort
      Just s -> return . read $ s

   simpleHTTP (nullConf { port = serverPort }) $ routing confMap


routing :: ConfMap -> ServerPart Response
routing confMap = msum
   [ dir "getShowList" $ getShowList confMap
   , dir "playVideo" $ playVideo
   , dir "delVideo" $ delVideo
   , serveDirectory DisableBrowsing ["index.html"] "site"
   ]


getShowList :: ConfMap -> ServerPart Response
getShowList confMap = do
   method GET

   liftIO $ putStrLn "Received getShowList request"

   -- FIXME Do this with better error handling
   let topDirs = splitList . fromJust $ lookup "topLevelDirs" confMap
   let vidExts = splitList . fromJust $ lookup "videoExtensions" confMap

   (errMsgs, programs) <- liftIO $ getAllPrograms topDirs vidExts

   liftIO $ mapM_ putStrLn errMsgs

   let json = encode (programs :: Programs)
   --liftIO $ BL.putStrLn json

   ok $ toResponse (json :: BL.ByteString)


bodyPolicy :: BodyPolicy
bodyPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)


playVideo :: ServerPart Response
playVideo = do
   method POST
   decodeBody bodyPolicy

   liftIO $ putStrLn "Received playVideo request"

   mbBody <- takeRequestBody =<< askRq
   liftIO $ case mbBody of
      Just b -> do
         let playpath = BL.unpack . unBody $ b
         --printf "path: %s\n" playpath

         _ <- runCommand $ printf "vlc \"%s\"" playpath
         return ()

      Nothing ->
         putStrLn "NO PATH! BAD!"

   ok $ toResponse ("got it" :: String)


delVideo :: ServerPart Response
delVideo = do
   method POST
   decodeBody bodyPolicy

   liftIO $ putStrLn "Received delVideo request"

   mbBody <- takeRequestBody =<< askRq
   liftIO $ case mbBody of
      Just b -> do
         let playpath = BL.unpack . unBody $ b
         --printf "deleting path: %s\n" playpath
         removeLink playpath

      Nothing ->
         putStrLn "NO PATH! BAD!"

   ok $ toResponse ("got it" :: String)


splitList :: String -> [String]
splitList s = splitRegex (mkRegex ";") s
