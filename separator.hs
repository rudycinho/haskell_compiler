import System.IO

main = do
    contents <- readFile "HelloWorld.java"
    let str =  separator (cleaner contents)
    return str

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

