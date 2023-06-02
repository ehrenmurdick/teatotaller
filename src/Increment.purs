module Increment
  ( update
  , view
  , init
  , Model
  , Msg(..)
  ) where

import Oak

import Prelude hiding (div)
import Effect

type Model = { number :: Int }

data Msg
  = Inc
  | Dec

instance showMsg :: Show Msg where
  show Inc = "Inc"
  show Dec = "Dec"

view :: Model -> Html Msg
view model = div []
  [ text "this is in the child"
  , div []
      [ button [ onClick Inc ] [ text "+" ]
      , div [] []
      , text $ show model.number
      , div [] []
      , button [ onClick Dec ] [ text "-" ]
      ]
  ]

update :: Msg -> Model -> Model
update msg model = case msg of
  Inc -> model { number = model.number + 1 }
  Dec -> model { number = model.number - 1 }

init :: Model
init = { number: 0 }
