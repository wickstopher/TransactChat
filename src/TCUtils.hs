-- TransactChat: Utility module
-- Christopher R. Wicks (wickstopher@gmail.com)

module TCUtils where

import TCData
import System.IO
import Data.Char
import Data.List.Split

-- Create tokens from a String, splitting on spaces
tokenize :: String -> [String]
tokenize = filter (not . null) . splitOn " "

-- Strip the first token, including all leading and trailing spaces, from
-- the String
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

-- Strip the first n tokens, including all leading and trailing spaces,
-- from the String
stripN :: Int -> String -> String
stripN 0 s = s
stripN n s = stripN (n-1) (stripFirst s)

-- Request and sanitize a line of input from the given Handle
sanitizeInput :: Handle -> IO String
sanitizeInput handle = do
    input <- hGetLine handle
    return (sanitize input)

-- sanitize a String (filter out unprintable characters)
sanitize :: String -> String
sanitize = filter (\s -> (isPrint s) || (s == '\t'))

-- Test for a valid username
validName :: String -> Bool
validName [] = False
validName name = ((length name) <= 10) && not (containsSpace name)

-- Test for whether a String contains space characters
containsSpace :: String -> Bool
containsSpace str = (length (filter (/= ' ') str)) /= (length str)
