-- Transact Chat: User functions and routines
-- Christopher R. Wicks (wickstopher@gmail.com)

module TCUser where

import TCData
import TCUtils
import Control.Concurrent
import Control.Concurrent.TxEvent
import System.IO

{- Main processing loops and functions for Users -}

-- Initialize a User's processing loops and return the newly spawned ThreadIds
initUser :: User -> SChan Message -> IO (ThreadId, ThreadId)
initUser user@(handle, name, userChan) serverChan = do
    hPutStrLn handle ("Welcome, " ++ name ++ 
        "!\nType ':help' for a list of available commands.")
    doPrompt handle
    tid1 <- forkIO (userMsgLoop user)
    tid2 <- forkIO (userInputLoop user serverChan)
    return (tid1, tid2)

-- Handles the receipt of messages on a User's channel
userMsgLoop :: User -> IO ()
userMsgLoop user@(handle, name, userChan) = do  
    msg <- sync (recvEvt userChan)
    case msg of (Msg s) -> hPutStrLn handle (('\r':s))
    doPrompt handle
    userMsgLoop user

-- Process User input and send appropriate requests to the server's channel
userInputLoop :: User -> SChan Message -> IO ()
userInputLoop user@(h, n, c) serverChan = do
    eof <- hIsEOF h
    if eof then do
        forkIO (sync (sendEvt serverChan (Request user Logout)))
        return ()
    else do
        doPrompt h
        input   <- sanitizeInput h
        message <- return (processInput user input)
        case message of (Just (Error s)) -> do (hPutStrLn h s); doPrompt h
                        (Just m)         -> do forkIO (sync (sendEvt serverChan m))
                                               return ()
                        otherwise        -> return ()
        userInputLoop user serverChan

-- Send a prompt to the Handle
doPrompt :: Handle -> IO ()
doPrompt handle = hPutStr handle "> "

-- Convert a line of user input into an appropriate Message.
processInput :: User -> String -> Maybe Message
processInput user input =
    let tokens = (tokenize input) in
        if null tokens 
            then Nothing
        else if (head (head tokens)) /= ':' 
            then Just (Request user (ChatMessage input))
        else if (head tokens) == ":quit"
            then Just (Request user Logout)
        else if (head tokens) == ":pm" && (length tokens) > 2
            then Just (Request user (PrivateMessage (head (tail tokens))
                (stripN 2 input)))
        else if (head tokens) == ":who"
            then Just (Request user Who)
        else if (head tokens) == ":help"
            then Just (Error helpText)
        else
            Just (Error "Unknown command")

-- Help text to display to the user when the :help command is entered
helpText :: String
helpText =
    "\n  :help        Display this help text.\n" ++
    "  :quit        Exit the chat server.\n" ++
    "  :pm <user>   Send a private message to <user>\n" ++
    "  :who         Display a list of currently logged-in users\n\n" ++
    "  To chat, simply type your message at the prompt and it will be\n" ++
    "  broadcast to all logged-in users.\n"

-- Broadcast a message to all Users in the list
broadcastMsg :: String -> [User] -> IO ()
broadcastMsg msg []    = return ()
broadcastMsg msg users = do
    msgUser msg (head users)
    broadcastMsg msg (tail users)

-- Send a message to the User
msgUser :: String -> User -> IO ThreadId
msgUser msg (_, _, userChan) = 
    forkIO (sync (sendEvt userChan (Msg msg)))

-- return a User's name
getName :: User -> String
getName (_, n, _) = n

-- return a User's Handle
getHandle :: User -> Handle
getHandle (h, _, _) = h

-- return the User from the list whose name matches the input String
getUser :: String -> [User] -> Maybe User
getUser userName [] = Nothing
getUser userName (u@(_, n, _):rest) 
    | userName == n = Just u
    | otherwise     = getUser userName rest

-- Filter the user whose name matches the input String from the list
filterSelf :: User -> [User] -> [User]
filterSelf (_, n1, _) = filter (\(_, n2, _) -> n1 /= n2 )

-- Return whether or not a User whose name matches the input String is
-- present in the list of Users
userLoggedIn :: String -> [User] -> Bool
userLoggedIn _ []     = False
userLoggedIn s (u@(_, n, _):rest)
    | s == n    = True
    | otherwise = userLoggedIn s rest
