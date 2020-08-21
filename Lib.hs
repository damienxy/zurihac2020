module Lib
  ( checkPalindrome
  )
where

import           Data.Char
import           Data.Maybe

-- Palindrome

checkPalindrome :: String -> String
checkPalindrome word = case isPalindrome word of
  Nothing    -> "Incorrect input"
  Just False -> "Unfortunately this is not a palindrome."
  Just True  -> "Hello, yes, nice, this is a palindrome."

isPalindrome :: String -> Maybe Bool
isPalindrome string = isOwnReverseMaybe $ rejectEmpty $ normalize string

isOwnReverse :: String -> Bool
isOwnReverse string = string == reverse string

isOwnReverseMaybe :: Maybe String -> Maybe Bool
isOwnReverseMaybe maybeString = case maybeString of
  Nothing     -> Nothing
  Just string -> Just $ isOwnReverse string

rejectEmpty :: String -> Maybe String
rejectEmpty word = case word of
  [] -> Nothing
  _  -> Just word

normalize :: String -> String
normalize string = filter notPunctuation $ filter notSpace $ toLowerCase string

notPunctuation :: Char -> Bool
notPunctuation x = not $ isPunctuation x

notSpace :: Char -> Bool
notSpace x = x /= ' '

toLowerCase :: String -> String
toLowerCase = map toLower


-- User database lookup

database :: [(Integer, String)]
database =
  [ (1, "Julie")
  , (2, "Chris")
  , (3, "Mei")
  , (4, "§)=%H//$a(§/s(§/k(§!/e))/$l?!(l)")
  ]

greetUser :: Integer -> String
greetUser record = fromMaybe "No user found" (getUser record)

getUser :: Integer -> Maybe String
getUser record = fmap ("Hello " ++) $ lookupUser record >>= normalizeUsername

lookupUser :: Integer -> Maybe String
lookupUser record = lookup record database

normalizeUsername :: String -> Maybe String
normalizeUsername string =
  removeSpaces string >>= removeNonAlphabetic >>= validateLength

removeSpaces :: String -> Maybe String
removeSpaces string = case filter (/= ' ') string of
  []     -> Nothing
  result -> Just result

removeNonAlphabetic :: String -> Maybe String
removeNonAlphabetic string = case filter isAlpha string of
  []     -> Nothing
  result -> Just result

validateLength :: String -> Maybe String
validateLength string = if length string > 30 then Nothing else Just string
