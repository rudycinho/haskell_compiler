module Scanner Test where

import Test.QuickCheck
import Scanner


readerTest1 = readerIntoString "aa++bcd<<e.d;"        == ["aa","++","bcd","<<","e",".","d",";"]
readerTest2 = readerIntoString "int a = hello ++;\n"  == ["int","a","=","hello","++",";"]
readerTest3 = readerIntoString "int a = #hello ++;\n" == ["error Symbol illegal"]

quotesTest1 = readerIntoString "int a = \"hello\" + \"world\";" == ["int","a","=","\"hello\"","+","\"world\";"]
quotesTest2 = readerIntoString "hello\"hello\""                 == ["hello","\"hello\""]
quotesTest3 = readerIntoString "\"hello\"hello"                 == ["\"hello\"","hello"]
quotesTest4 = readerIntoString "int a = \"hello + \"world\""    == ["int","a","=","\"hello + \"","world","\""] 
quotesTest5 = readerIntoString "int a = \"hello\" + \"world;"   == ["int","a","=","\"hello\"","+","\"world;"]


main = do
    quickCheck readerTest1
    quickCheck readerTest2
    quickCheck readerTest3
    quickCheck quotesTest1