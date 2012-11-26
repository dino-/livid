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
   --[ dir "xml" $ fileServing
   --[ notFound $ toResponse ("Unknown resource" :: String)
   [ unknownResource
   ]


unknownResource :: ServerPart Response
unknownResource = do
   let errMsg = "Unknown resource"
   liftIO $ putStrLn errMsg
   notFound $ toResponse (errMsg :: String)


{-
getSomething :: ServerPart Response
getSomething = do
   liftIO $ putStrLn "\nsomething handled"

   ok $ toResponse $ ("<foo><bar>baz</bar></foo>" :: String)
-}


{-
bodyPolicy :: BodyPolicy
bodyPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)
-}


{-
fileServing :: ServerPart Response
fileServing = do
   decodeBody bodyPolicy

   xmlPath <- path $ \(filename :: String) ->
      return $ "resources" </> filename

   liftIO $ putStrLn $ printf "trying to serve file: %s" xmlPath

   fileFound <- liftIO $ doesFileExist xmlPath
   if (fileFound)
      then serveFile (asContentType "text/xml") xmlPath
      else notFound $ toResponse
         ((printf "File not available: %s" xmlPath) :: String)
-}
