-- TransactChat: top level module
-- Christopher R. Wicks (wickstopher@gmail.com)

module TransactChat where

import TCData
import TCLogging
import TCUtils
import TCUser
import TCServer

import Control.Concurrent
import Control.Concurrent.TxEvent
import Network.Socket
import System.Environment
import System.IO
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    args <- getArgs
    port <- return (processArgs args)
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 5
    serverChan <- sync newSChan
    forkIO (serverLoop [] HM.empty serverChan)
    logMessage ("TransactChat server initialized on port " ++ (show port))
    socketLoop sock serverChan

-- Process command-line arguments.
processArgs :: [String] -> PortNumber
processArgs (s:[]) = fromIntegral ((read s)::Int)
processArgs _      = 4242

{- Listen for new connections and initialize login routines -}
socketLoop :: Socket -> SChan Message -> IO ()
socketLoop sock serverChan = do 
    (uSock, _) <- accept sock
    uHandle    <- socketToHandle uSock ReadWriteMode
    forkIO (login uHandle serverChan)
    socketLoop sock serverChan
