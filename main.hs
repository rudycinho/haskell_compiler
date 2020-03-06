module Main where

import Scanner
import System.IO

main = do
    contents <- readFile "HelloWorld.java"
    let str =  classifier (separator (cleaner contents))
    return str
