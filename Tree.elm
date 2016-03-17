module Tree (..) where

import Mouse
import Html exposing (text, div, ul, li)
import Html.Attributes exposing (class)


type Tree a
  = Node a (Tree a) (Tree a)
  | Empty


treeOfDepth : Int -> Tree String
treeOfDepth i =
  case i of
    0 ->
      Empty

    1 ->
      Node "1" Empty Empty

    _ ->
      let
        nextDepth =
          i - 1
      in
        Node (toString i) (treeOfDepth nextDepth) (treeOfDepth nextDepth)


renderTree : Tree String -> Html.Html
renderTree tree =
  case tree of
    Empty ->
      ul [] [ li [ class "empty-node" ] [ text "empty node" ] ]

    Node name t1 t2 ->
      ul [ class "node" ] [ li [] [ text name, renderTree t1, renderTree t2 ] ]


view : Int -> Html.Html
view i =
  div
    [ class "tree-root" ]
    [ renderTree (treeOfDepth i) ]


boundMouse : Int -> Int
boundMouse i =
  if i > 600 then
    6
  else
    ceiling <| i / 100


main : Signal Html.Html
main =
  Signal.map view <| Signal.map boundMouse Mouse.y
