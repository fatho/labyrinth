module Util where

import Dict
import Keyboard
import List
import List ((::))
import Signal
import Signal (Signal, (<~), (~))

-- * Utility functions

keyPresses : Keyboard.KeyCode -> Signal ()
keyPresses key = always () <~ Signal.keepIf identity False (Keyboard.isDown key)

-- | Similar to Haskell's maybeToList
maybeToList : Maybe a -> List a
maybeToList m = case m of
  Just x -> [x]
  Nothing -> []

-- | Haskells `ap` specialized to lists.
ap : List (a -> b) -> List a -> List b
ap fs xs = List.concatMap (flip List.map xs) fs

-- | Haskell'S mapAccumL
mapAccumL : (a -> acc -> (b, acc)) -> acc -> List a -> (List b, acc)
mapAccumL f acc xxs = case xxs of
  [] -> ([], acc)
  (x::xs) -> 
    let (y, acc') = f x acc
        (ys, acc'') = mapAccumL f acc' xs
    in (y::ys, acc'')

cartProd : List a -> List b -> List (a,b)
cartProd x y = (,) `List.map` x `ap` y

-- | Haskell's List.singleton
singleton : a -> List a
singleton x = [x]

-- | Haskell's List.last
last : List a -> a
last = List.reverse >> List.head

-- | Haskell's List.init
init : List a -> List a
init xs = case xs of 
  [] -> List.tail []
  [x] -> []
  (x::xs) -> x :: init xs

-- | Haskell's traverse.
traverse : (a -> Signal b) -> List a -> Signal (List b)
traverse f xs = case xs of
  [] -> Signal.constant []
  (x::xs) -> (::) <~ f x ~ traverse f xs

-- | Subscribe a list of channels and get a signal of lists.
subscribeMany : List (Signal.Channel a) -> Signal (List a)
subscribeMany chs = case chs of
  [] -> Signal.constant []
  (x::xs) -> (::) <~ Signal.subscribe x ~ subscribeMany xs

-- | When you know there must be an element.
unsafeGet : comparable -> Dict.Dict comparable v -> v
unsafeGet key = Dict.get key >> maybeToList >> List.head
