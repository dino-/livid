{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson ( encode )
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics ( Generic )
import Happstack.Server
import System.Environment ( getArgs )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.Printf

import Vidproj.Directory


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

   eps <- liftIO $ getPrograms "testsuite/vid/nonempty1"
   let programs = case eps of
         Right ps -> ps
         Left err -> error err

   let json = encode (programs :: Programs)
   liftIO $ BL.putStrLn json

   ok $ toResponse (json :: BL.ByteString)
