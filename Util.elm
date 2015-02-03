module Util where

import Dict
import Graphics.Element as GEl
import Html
import Html.Attributes as Html
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
subscribeMany = traverse Signal.subscribe

-- | When you know there must be an element.
unsafeGet : comparable -> Dict.Dict comparable v -> v
unsafeGet key = Dict.get key >> maybeToList >> List.head

-- CSS to make element unselectable
unselectable : Html.Attribute
unselectable = Html.style
  [ ("-webkit-touch-callout", "none")
  , ("-webkit-user-select", "none")
  , ("-khtml-user-select", "none")
  , ("-moz-user-select", "none")
  , ("-ms-user-select", "none")
  , ("user-select", "none")
  ]

-- | Sets the CSS class of an element.
withClass : String -> GEl.Element -> GEl.Element
withClass class el =
  let (w,h) = GEl.sizeOf el
  in Html.div [ Html.class class ] [ Html.fromElement el ] |> Html.toElement w h

-- | Wraps an element in a div tag which prevents text selections.
makeUnselectable : GEl.Element -> GEl.Element
makeUnselectable el =
  let (w,h) = GEl.sizeOf el
  in Html.div [ unselectable ] [ Html.fromElement el ] |> Html.toElement w h

--mixRGB : Float -> Color.Color -> Color.Color -> Color.Color
--mixRGB mix c1 c2 = 
