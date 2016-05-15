-- TransactChat: Utility module
-- Christopher R. Wicks (wickstopher@gmail.com)

module TCUtils where

import System.IO
import Data.Char
import Data.List.Split
import TCData

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
    input <- (hGetLine handle)
    return (sanitize (init input))

-- sanitize a String: only allow alpha-numeric and spaces chars
sanitize :: String -> String
sanitize = filter (\s -> (isAscii s) && (isPrint s || s == '\t'))

-- Test for a valid username
validName :: String -> Bool
validName [] = False
validName name = ((length name) <= 10) && not (containsNonAlpha name)

-- Test for whether a String contains space characters
containsNonAlpha :: String -> Bool
containsNonAlpha str = (length (filter isAlphaNum str)) /= (length str)
