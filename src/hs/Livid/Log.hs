-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Livid.Log
   ( initLogging, lname

   -- Re-exported from System.Log
   , Priority (..), debugM, infoM, noticeM, warningM, errorM
   , criticalM, alertM, emergencyM, logM
   , setLevel
   )
   where

import System.IO ( stderr, stdout )
import System.Log.Formatter ( simpleLogFormatter )
import System.Log.Handler ( setFormatter )
import System.Log.Handler.Simple ( streamHandler )
import System.Log.Logger hiding ( setLevel )
import qualified System.Log.Logger as L


lname :: String
lname = "normal-output"


{- Set up logging
-}
initLogging :: Priority -> IO ()
initLogging logPriority = do
  -- Remove the root logger's default handler that writes every
  -- message to stderr!
  updateGlobalLogger rootLoggerName removeHandler

  -- Set up our logger
  let fmt = flip setFormatter $ simpleLogFormatter "[$prio] $msg"
  outH <- fmt <$> streamHandler stdout DEBUG
  errH <- fmt <$> streamHandler stderr CRITICAL
  updateGlobalLogger lname $ setHandlers [outH, errH]
  setLevel logPriority


setLevel :: Priority -> IO ()
setLevel = updateGlobalLogger lname . L.setLevel
