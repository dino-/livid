{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson ( encode )
import qualified Data.ByteString.Lazy.Char8 as BL
import Happstack.Server
import System.Environment ( getArgs )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import System.Posix.Files ( removeLink )
import System.Process ( runCommand )
import Text.Printf

import Vidproj.Program


defaultPort :: Int
defaultPort = 8081


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   args <- getArgs
   serverPort <- case args of
      []     -> do
         putStrLn $ printf "Using default port %d" defaultPort
         putStrLn "Specify a different port like this: ad-testsrv PORT"
         return defaultPort
      (p':_) -> return . read $ p'

   simpleHTTP (nullConf { port = serverPort }) routing


routing :: ServerPart Response
routing = msum
   [ dir "getShowList" $ getShowList
   , dir "playVideo" $ playVideo
   , dir "delVideo" $ delVideo
   , serveDirectory DisableBrowsing ["index.html"] "site"
   ]


getShowList :: ServerPart Response
getShowList = do
   method GET

   liftIO $ putStrLn "Received getShowList request"

   (errMsgs, programs) <- liftIO $ getAllPrograms
      [ "testsuite/vid/empty"
      , "testsuite/vid/doesnotexist"
      , "testsuite/vid/nonempty2"
      , "testsuite/vid/nonempty1"
      ]

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
