-- TransactChat Logging module
-- Christopher R. Wicks (wickstopher@gmail.com)

module TCLogging where

import TCData
import Data.Time

{- Logging functions -}

-- Given a Message, construct and display an appropriate message on the 
-- server terminal
makeLogMessage :: Maybe Message -> IO ()
makeLogMessage message = case message of 
    Just (Request (_, n, _) (Login _)) -> 
        logMessage ("User " ++ n ++ " has logged in")
    Just (Request (_, n, _) Logout)    -> 
        logMessage ("User " ++ n ++ " has logged out")
    otherwise -> return ()

-- Display a log message on the server terminal
logMessage :: String -> IO ()
logMessage msg = do
    utcTime <- getCurrentTime
    time    <- utcToLocalZonedTime utcTime
    putStrLn ("[" ++ (show time) ++ "] " ++ msg)