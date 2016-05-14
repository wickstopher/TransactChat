module TCServer where

import TCData
import TCUtils
import TCLogging
import TCUser

import Control.Concurrent
import Control.Concurrent.TxEvent
import System.IO
import qualified Data.HashMap.Strict as HM


{- Login routine for a new user -} 
login :: Handle -> SChan Message -> IO ()
login handle serverChan = do
    hSetBuffering handle NoBuffering
    hPutStr handle "Please enter your name: "
    name <- sanitizeInput handle
    userChan  <- sync newSChan
    loginChan <- sync newSChan
    sync (sendEvt serverChan (Request (handle, name, userChan) (Login loginChan)))
    message <- sync (recvEvt loginChan)
    case message of (Failed error) -> do
                        hPutStrLn handle error
                        login handle serverChan
                    Success -> return ()

{- Main server processing loop -}
serverLoop :: [User] -> HM.HashMap String (ThreadId, ThreadId) -> SChan Message -> IO ()
serverLoop users tidMap serverChan = do
    message <- sync (recvEvt serverChan)
    
    (newUsers, newMap, logmsg) <- case message of 
        Request user (ChatMessage msg) -> do 
            forkIO (broadcastMsg ((getName user) ++ ": " ++ msg) users)
            return (users, tidMap, Nothing)
        Request user (PrivateMessage uName msg) -> do
            case (getUser uName users) of
                (Just u) -> msgUser ((getName user) ++ " [private]: " ++ msg) u
                Nothing  -> msgUser ("User " ++ uName ++ " not logged in") user
            return (users, tidMap, Nothing)
        Request user Who -> do
            msgUser (makeUserList (filterSelf user users)) user
            return (users, tidMap, Nothing)
        Request user (Login loginChan) ->
            if not (validName (getName user)) then do
                loginFailed loginChan ("That is not a valid username.\n" ++
                    "Names must be between 1 and 10 characters and contain no spaces.")
                return (users, tidMap, Nothing)
            else if (userLoggedIn (getName user) users) then do
                loginFailed loginChan "A user with that name is already logged in!"
                return (users, tidMap, Nothing)
            else do
                forkIO (sync (sendEvt loginChan Success))
                threadIds <- initUser user serverChan
                forkIO (broadcastMsg ("*** " ++ (getName user) 
                            ++ " has entered chat! ***") users)
                return ((user:users), (HM.insert (getName user) threadIds tidMap), (Just message))
        Request user Logout -> do
            remaining    <- return (filterSelf user users)
            (tid1, tid2) <- return (tidMap HM.! (getName user))
            forkIO (do killThread tid1; killThread tid2; hClose (getHandle user))
            forkIO (broadcastMsg ("*** " ++ (getName user) 
                        ++ " has departed. ***") remaining)
            return (remaining, (HM.delete (getName user) tidMap), (Just message))
    makeLogMessage logmsg 
    serverLoop newUsers newMap serverChan


{- Helper functions for the main server loop -}

-- Send a login failed response to the LoginResponse channel
loginFailed :: SChan LoginResponse -> String -> IO ThreadId
loginFailed loginChan msg = forkIO (sync (sendEvt loginChan (Failed msg)))

-- Construct a displayable String containing the list of logged-in Users
makeUserList :: [User] -> String
makeUserList [] = "Nobody else is logged in at the moment."
makeUserList users = 
    let makeList [] = ""
        makeList ((_, n, _):rest) = "* " ++ n ++ "\n" ++ (makeList rest)
    in "\n\n-----------------\n| Current Users |\n-----------------\n\n" ++ makeList users