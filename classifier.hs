module classifier where

data Symbol = Word String | Operator String | Identifier String | Separator String | LiteralInteger String

classifier :: [String] -> [Symbol]
classifier [] = []
classifier (x:xs) = clsf : (classifier xs)

cls :: String -> Symbol
cls x | 



