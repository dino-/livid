{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Happstack.Server
--import System.Directory ( doesFileExist )
import System.Environment ( getArgs )
--import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.JSON
import Text.Printf


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
   , serveDirectory DisableBrowsing ["index.html"] "site"
   ]


getShowList :: ServerPart Response
getShowList = do
   method GET

   liftIO $ putStrLn "Received getShowList request"


   -- FIXME loading dev data file for now
   jsonRaw <- liftIO $ fmap (unlines . tail . lines) $
      readFile $ "../client/dev/data.js"

   let json :: JSValue = case decodeStrict jsonRaw of
         Ok j -> j
         Error err -> error err

   let jsonEncoded = encodeStrict json


   ok $ toResponse (jsonEncoded :: String)
