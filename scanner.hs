module Scanner where

import Data.Char

separator :: [Char] -> [[Char]]
separator xs = sep xs [] []

sep :: [Char] -> [Char] -> [[Char]] -> [[Char]]
sep [] [] zss = zss
sep [] ys zss = zss ++ [ys]
sep (x:xs) [] zss | is_symbol x = sep xs [] (zss++[[x]])
                  | is_space x  = sep xs [] zss
                  | otherwise   = sep xs [x] zss
sep (x:xs) ys zss | is_symbol x = sep xs [] (zss++[ys]++[[x]])
                  | is_space x  = sep xs [] (zss++[ys])
                  | otherwise   = sep xs (ys++[x]) zss

cleaner :: [Char] -> [Char]
cleaner [] = []
cleaner (x:xs) | (x=='\n' || x=='\t') = ' ' : (cleaner xs)
               | otherwise = x : (cleaner xs)

is_symbol :: Char -> Bool
is_symbol x = (x=='{') ||
               (x=='}') ||
               (x=='[') ||
               (x==']') ||
               (x=='(') ||
               (x==')') ||
               (x=='=') ||
               (x=='+') ||
               (x=='-') ||
               (x=='*') ||
               (x=='/') ||
               (x=='"') ||
               (x==';')

is_space :: Char -> Bool
is_space x = x == ' '



data Symbol = StopWord String | Operator String | Identifier String | Separator String | LiteralInteger String | Expression String deriving (Show)

classifier :: [String] -> [Symbol]
classifier [] = []
classifier (x:xs) = (clsf x): (classifier xs)

clsf :: String -> Symbol
clsf x | is_operator x  = Operator x
       | is_separator x = Separator x
       | checkNumber x = LiteralInteger x
       | is_stop_word x = StopWord x
       | otherwise = Expression x

is_operator :: String -> Bool
is_operator x = (x=="+")  ||
                (x=="-")  ||
                (x=="*")  ||
                (x=="/")  ||
                (x=="+=") ||
                (x=="-=") ||
                (x=="*=") ||
                (x=="/=")

is_separator :: String -> Bool
is_separator x = (x=="{") ||
                 (x=="}") ||
                 (x=="[") ||
                 (x=="]") ||
                 (x=="(") ||
                 (x==")")

is_stop_word :: String -> Bool
is_stop_word x = (x=="true") ||
                 (x=="false") ||
                 (x=="null") ||
                 (x=="abstract") ||
                 (x=="assert") ||
                 (x=="boolean") ||
                 (x=="break") ||
                 (x=="byte") ||
                 (x=="case") ||
                 (x=="catch") ||
                 (x=="char") ||
                 (x=="class") ||
                 (x=="const") ||
                 (x=="continue") ||
                 (x=="default") ||
                 (x=="do") ||
                 (x=="double") ||
                 (x=="else") ||
                 (x=="enum") ||
                 (x=="extends") ||
                 (x=="final") ||
                 (x=="finally") ||
                 (x=="float") ||
                 (x=="for") ||
                 (x=="goto") ||
                 (x=="if") ||
                 (x=="implements") ||
                 (x=="import") ||
                 (x=="instanceof") ||
                 (x=="int") ||
                 (x=="interface") ||
                 (x=="long") ||
                 (x=="native") ||
                 (x=="new") ||
                 (x=="package") ||
                 (x=="private") ||
                 (x=="protected") ||
                 (x=="public") ||
                 (x=="return") ||
                 (x=="short") ||
                 (x=="static") ||
                 (x=="strictfp") ||
                 (x=="super") ||
                 (x=="switch") ||
                 (x=="synchronized") ||
                 (x=="this") ||
                 (x=="throw") ||
                 (x=="throws") ||
                 (x=="transient") ||
                 (x=="try") ||
                 (x=="void") ||
                 (x=="volatile") ||
                 (x=="while") 
               
checkNumber :: String -> Bool
checkNumber = all isDigit