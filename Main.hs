module Main where

import           Lib                            ( checkPalindrome )

main :: IO ()
main = do
  print "Please insert a word or phrase:"
  word <- getLine
  print $ checkPalindrome word
