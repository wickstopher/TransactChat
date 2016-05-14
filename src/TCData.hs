-- Data structure definitions for TransactChat
-- Christopher R. Wicks (wickstopher@gmail.com)

module TCData where

import Control.Concurrent
import Control.Concurrent.TxEvent
import Network.Socket
import System.IO

data Message            = Request User ServerRequestType | Error String
data ServerRequestType  = ChatMessage String | 
                          PrivateMessage String String | 
                          Who | 
                          Login (SChan LoginResponse) | 
                          Logout
data LoginResponse      = Success | Failed String
data UserMessage        = Msg String 
type User               = (Handle, String, SChan UserMessage)
