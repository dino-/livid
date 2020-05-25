-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson ( encode )
import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Version ( showVersion )
import Data.Map
-- FIXME
import Data.Maybe ( fromJust )
import Happstack.Server
import Paths_livid_srv ( version )
import Prelude hiding ( lookup )
import System.Environment ( getEnv )
import System.FilePath ( (</>) )
import System.IO
  ( BufferMode ( NoBuffering )
  , hSetBuffering, stdout, stderr
  )
import System.Posix.Files ( removeLink )
import System.Process ( runCommand )
import Text.Printf
import Text.Regex ( mkRegex, splitRegex )

import Livid.Conf ( ConfMap, lookupReadWithDefault, parseToMap )
import Livid.Log ( debugM, errorM, infoM, initLogging, lname, noticeM,
  Priority (NOTICE), setLevel )
import Livid.Program


defaultLogPriority :: Priority
defaultLogPriority = NOTICE


defaultHttpPort :: Int
defaultHttpPort = 8082


defaultConfFile :: IO FilePath
defaultConfFile = (</> ".config" </> "livid.conf") <$> getEnv "HOME"


main :: IO ()
main = do
  -- No buffering, it messes with the order of output
  mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

  confMap <- loadConf

  let logPriority = readLogPriority confMap
  initLogging logPriority
  noticeM lname $ printf "Logging started with priority %s" (show logPriority)

  let httpPort = lookupReadWithDefault Nothing "httpPort" confMap
  noticeM lname $ printf "Starting server on port %d" httpPort

  simpleHTTP (nullConf { port = httpPort }) $ routing


routing :: ServerPart Response
routing = do
  confMap <- liftIO $ loadConf

  liftIO $ setLevel . readLogPriority $ confMap

  msum
    [ dir "getShowList" $ getShowList confMap
    , dir "playVideo" $ playVideo confMap
    , dir "delVideo" $ delVideo
    , dir "getVersion" $ getVersion
    , serveDirectory DisableBrowsing ["index.html"] "site"
    ]


loadConf :: IO ConfMap
loadConf = parseToMap <$> (readFile =<< defaultConfFile)


readLogPriority :: ConfMap -> Priority
readLogPriority confMap = lookupReadWithDefault (Just defaultLogPriority) "logPriority" confMap


getShowList :: ConfMap -> ServerPart Response
getShowList confMap = do
  method GET

  liftIO $ noticeM lname "Received getShowList request"

   -- FIXME Do this with better error handling
  let topDirs = splitList . fromJust $ lookup "topLevelDirs" confMap
  let vidExts = splitList . fromJust $ lookup "videoExtensions" confMap

  (errMsgs, programs) <- liftIO $ getAllPrograms topDirs vidExts

  liftIO $ mapM_ (errorM lname) errMsgs

  let json = encode (programs :: Programs)
  liftIO $ debugM lname $ BL.toString json

  ok $ toResponse (json :: BL.ByteString)


bodyPolicy :: BodyPolicy
bodyPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)


playVideo :: ConfMap -> ServerPart Response
playVideo confMap = do
  method POST
  decodeBody bodyPolicy

  liftIO $ noticeM lname "Received playVideo request"

  mbBody <- takeRequestBody =<< askRq
  liftIO $ case mbBody of
    Just b -> do
      let playpath = BL.toString . unBody $ b
      infoM lname $ printf "Playing path: %s\n" playpath

      let playbackCommand =
               fromJust $ lookup "playbackCommand" confMap
      _ <- runCommand $ printf "%s \"%s\"" playbackCommand playpath
      return ()

    Nothing ->
      errorM lname "NO PATH! BAD!"

  ok $ toResponse ("got it" :: String)


delVideo :: ServerPart Response
delVideo = do
  method POST
  decodeBody bodyPolicy

  liftIO $ noticeM lname "Received delVideo request"

  mbBody <- takeRequestBody =<< askRq
  liftIO $ case mbBody of
    Just b -> do
      let playpath = BL.toString . unBody $ b
      infoM lname $ printf "Deleting path: %s\n" playpath
      removeLink playpath

    Nothing ->
      errorM lname "NO PATH! BAD!"

  ok $ toResponse ("got it" :: String)


splitList :: String -> [String]
splitList s = splitRegex (mkRegex ";") s


getVersion :: ServerPart Response
getVersion = do
  method GET

  ok . toResponse . showVersion $ version
