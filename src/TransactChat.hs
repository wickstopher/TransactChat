module TransactChat where

import Control.Concurrent
import Control.Concurrent.TxEvent
import Control.Exception
import Data.Char
import Data.List.Split
import Data.Time
import Network.Socket
import System.Environment
import System.IO
import qualified Data.HashMap.Strict as HM

data Message            = Request User ServerRequestType | Error String
data ServerRequestType  = ChatMessage String | 
                          PrivateMessage String String | 
                          Who | 
                          Login (SChan LoginResponse) | 
                          Logout
data LoginResponse      = Success | Failed String
data UserMessage        = Msg String 
type User               = (Handle, String, SChan UserMessage)

getName :: User -> String
getName (_, n, _) = n

getHandle :: User -> Handle
getHandle (h, _, _) = h

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

processArgs :: [String] -> PortNumber
processArgs (s:[]) = fromIntegral ((read s)::Int)
processArgs _      = 4242

socketLoop :: Socket -> SChan Message -> IO ()
socketLoop sock serverChan = do 
    (uSock, _) <- accept sock
    uHandle    <- socketToHandle uSock ReadWriteMode
    forkIO (login uHandle serverChan)
    socketLoop sock serverChan

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

makeLogMessage :: Maybe Message -> IO ()
makeLogMessage message = case message of 
    Just (Request (_, n, _) (Login _)) -> 
        logMessage ("User " ++ n ++ " has logged in")
    Just (Request (_, n, _) Logout)    -> 
        logMessage ("User " ++ n ++ " has logged out")
    otherwise -> return ()

logMessage :: String -> IO ()
logMessage msg = do
    utcTime <- getCurrentTime
    time    <- utcToLocalZonedTime utcTime
    putStrLn ("[" ++ (show time) ++ "] " ++ msg)

loginFailed :: SChan LoginResponse -> String -> IO ThreadId
loginFailed loginChan msg = forkIO (sync (sendEvt loginChan (Failed msg)))

userLoggedIn :: String -> [User] -> Bool
userLoggedIn _ []     = False
userLoggedIn s (u@(_, n, _):rest)
    | s == n    = True
    | otherwise = userLoggedIn s rest

validName :: String -> Bool
validName [] = False
validName name = ((length name) <= 10) && not (containsSpace name)

containsSpace :: String -> Bool
containsSpace str = (length (filter (/= ' ') str)) /= (length str) 

makeUserList :: [User] -> String
makeUserList [] = "Nobody else is logged in at the moment."
makeUserList users = 
    let makeList [] = ""
        makeList ((_, n, _):rest) = "* " ++ n ++ "\n" ++ (makeList rest)
    in "\n\n-----------------\n| Current Users |\n-----------------\n\n" ++ makeList users

broadcastMsg :: String -> [User] -> IO ()
broadcastMsg msg []    = return ()
broadcastMsg msg users = do
    msgUser msg (head users)
    broadcastMsg msg (tail users)

msgUser :: String -> User -> IO ThreadId
msgUser msg (_, _, userChan) = 
    forkIO (sync (sendEvt userChan (Msg msg)))

initUser :: User -> SChan Message -> IO (ThreadId, ThreadId)
initUser user@(handle, name, userChan) serverChan = do
    hPutStrLn handle ("Welcome, " ++ name ++ 
        "!\nType ':help' for a list of available commands.")
    doPrompt handle
    tid1 <- forkIO (userMsgLoop user)
    tid2 <- forkIO (userInputLoop user serverChan)
    return (tid1, tid2)

userMsgLoop :: User -> IO ()
userMsgLoop user@(handle, name, userChan) = do  
    msg <- sync (recvEvt userChan)
    case msg of (Msg s) -> hPutStrLn handle (('\r':s))
    doPrompt handle
    userMsgLoop user

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

tokenize :: String -> [String]
tokenize = filter (not . null) . splitOn " "

stripFirst :: String -> String
stripFirst []     = []
stripFirst str    = let
    -- strip leading spaces
    state1 (x:xs)
        | x == ' '  = state1 xs
        | otherwise = state2 xs
    -- strip first string of non-space chars
    state2 [] = []
    state2 (x:xs)
        | x /= ' '  = state2 xs
        | otherwise = state3 xs
    -- strip remaining spaces up until next string of non-space chars
    state3 [] = []
    state3 (x:xs)
        | x /= ' '  = (x:xs)
        | otherwise = state3 xs 
    in state1 str

stripN :: Int -> String -> String
stripN 0 s = s
stripN n s = stripN (n-1) (stripFirst s)

getUser :: String -> [User] -> Maybe User
getUser userName [] = Nothing
getUser userName (u@(_, n, _):rest) 
    | userName == n = Just u
    | otherwise     = getUser userName rest

filterSelf :: User -> [User] -> [User]
filterSelf (_, n1, _) = filter (\(_, n2, _) -> n1 /= n2 )

sanitizeInput :: Handle -> IO String
sanitizeInput handle = do
    input <- hGetLine handle
    return (sanitize input)

sanitize :: String -> String
sanitize = filter (\s -> (isPrint s) || (s == '\t'))

doPrompt handle = hPutStr handle "> "

helpText :: String
helpText =
    "\n  :help        Display this help text.\n" ++
    "  :quit        Exit the chat server.\n" ++
    "  :pm <user>   Send a private message to <user>\n" ++
    "  :who         Display a list of currently logged-in users\n\n" ++
    "  To chat, simply type your message at the prompt and it will be\n" ++
    "  broadcast to all logged-in users.\n"
