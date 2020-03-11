module Scanner where

import Data.Char

readerIntoString :: [Char] -> [[Char]]
readerIntoString xs = reader xs [] []

reader :: [Char] -> [Char] -> [[Char]] -> [[Char]]
reader [] [] zss         = zss

reader [] [] zss = zss
reader [] ys zss = zss ++ [ys]
reader (x:xs) [] zss | isSpace      x = reader xs [] zss
                     | isSeparator_ x = reader xs [] (zss ++ [[x]])
                     | isAlphaNum   x = reader xs [x] zss
                     | isSymbol_    x = reader xs [x] zss
                     | otherwise = ["error Symbol illegal"]
reader (x:xs) yys@(y:ys) zss | isSpace      x                      = reader xs [] (zss ++ [yys])
                             | isSeparator_ x                      = reader xs [] (zss ++ [yys] ++ [[x]])
                             | (isAlphaNum  x) && (isAlphaNum  y)  = reader xs (yys++[x]) zss
                             | (isSymbol_   x) && (isSymbol_   y)  = reader xs (yys++[x]) zss
                             | (isAlphaNum  x) && (isSymbol_   y)  = reader xs [x] (zss++[yys])
                             | (isSymbol_   x) && (isAlphaNum  y)  = reader xs [x] (zss++[yys])
                             | otherwise = ["error Symbol illegal"]


isReservedWord :: String -> Bool
isReservedWord word = word `elem` reservedWords

isOperator :: String -> Bool
isOperator word = 
    word `elem` unaryPostfixOperators  ||
    word `elem` prefixPostfixOperators ||
    word `elem` arithmeticOperators    ||
    word `elem` shiftOperators         ||
    word `elem` relationalOperators    ||
    word `elem` bitwiseOperators       ||
    word `elem` logicalOperators       ||
    word `elem` ternaryOperators       ||
    word `elem` assignmentOperators

isSeparator_ :: Char -> Bool
isSeparator_ character = character `elem` separators

isSymbol_ :: Char -> Bool
isSymbol_ character = character `elem` symbols


reservedWords = ["abstract","assert","boolean","break","byte","case","catch","char","class","const","continue","default","do","double","else","enum","extends","final","finally","float","for","if","goto","implements","import","instanceof","int","interface","long","native","new","package","private","protected","public","return","short","static","strictfp","super","switch","synchronized","this","throw","throws","transient","try","void","volatile","while","true","false","null","var","const","goto"]

unaryPostfixOperators = ["++","--"]
prefixPostfixOperators= ["++","--","+","-","~"] 
arithmeticOperators = ["*","/","%","+","-"]
shiftOperators      = ["<<",">>",">>>"]
relationalOperators = ["<",">","<=",">=","instanceof","==","!="]
bitwiseOperators    = ["&","^","|"]
logicalOperators    = ["!","&&","||"]
ternaryOperators    = ["?",":"]
assignmentOperators = ["=","+=","-=","*=","/=","%=","&=","^=","|=","<<="," >>= ",">>>="]

separators = ['(',')','{','}','[',']',';',',','.']

symbols = ['+','-','*','/','%','<','>','&','|','!','~','?',':','^','=']