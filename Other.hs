module Other where

-- Reimplementation of some of Prelude's functions as exercise

-- map
myMap :: (a -> a) -> [a] -> [a]
myMap func list = case list of
  []       -> []
  (x : xs) -> func x : myMap func xs

-- filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter predicate list = case list of
  [] -> []
  (x : xs) ->
    if predicate x then x : myFilter predicate xs else myFilter predicate xs

-- any
myAny :: (a -> Bool) -> [a] -> Bool
myAny pred list = case list of
  []       -> False
  (x : xs) -> pred x || myAny pred xs

-- all
myAll :: (a -> Bool) -> [a] -> Bool
myAll pred list = case list of
  []       -> True
  (x : xs) -> pred x && myAll pred xs

-- all 
myAll2 :: (a -> Bool) -> [a] -> Bool
myAll2 pred = foldr (\x y -> pred x && y) True

-- head
myHead :: [a] -> a
myHead (x : xs) = x

-- tail
myTail :: [a] -> [a]
myTail list = case list of
  []       -> []
  (x : xs) -> xs
