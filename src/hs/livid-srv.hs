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


version :: String
version = "1.4"


defaultPort :: Int
defaultPort = 8082


defaultConfFile :: FilePath
defaultConfFile = "livid.conf"


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   confMap <- loadConf
   serverPort <- case (lookup "httpPort" confMap) of
      Nothing -> do
         putStrLn $ printf "Using default port %d" defaultPort
         putStrLn "Specify a different port like this: ad-testsrv PORT"
         return defaultPort
      Just s -> return . read $ s

   simpleHTTP (nullConf { port = serverPort }) $ routing


routing :: ServerPart Response
routing = do
   confMap <- liftIO $ loadConf
   msum
      [ dir "getShowList" $ getShowList confMap
      , dir "playVideo" $ playVideo confMap
      , dir "delVideo" $ delVideo
      , dir "getVersion" $ getVersion
      , serveDirectory DisableBrowsing ["index.html"] "site"
      ]


loadConf :: IO ConfMap
loadConf = fmap parseToMap $ readFile defaultConfFile


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


playVideo :: ConfMap -> ServerPart Response
playVideo confMap = do
   method POST
   decodeBody bodyPolicy

   liftIO $ putStrLn "Received playVideo request"

   mbBody <- takeRequestBody =<< askRq
   liftIO $ case mbBody of
      Just b -> do
         let playpath = BL.unpack . unBody $ b
         --printf "path: %s\n" playpath

         let playbackCommand =
               fromJust $ lookup "playbackCommand" confMap
         _ <- runCommand $ printf "%s \"%s\"" playbackCommand playpath
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


getVersion :: ServerPart Response
getVersion = do
   method GET

   ok $ toResponse (version :: String)
