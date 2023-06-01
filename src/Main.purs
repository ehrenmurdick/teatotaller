module Main (main) where

import Oak
import Increment as Increment

import Prelude
  ( Unit
  , bind
  , map
  , mempty
  )
import Effect

type Model =
  { increment :: Increment.Model
  , message :: String
  }

data Msg
  = IncrementMsg Increment.Msg
  | Other

view :: Model -> Html Msg
view model = div []
  [ text model.message
  , (map IncrementMsg (Increment.view model.increment))
  , text "this is the parent app"
  , div [] [ button [ onClick Other ] [ text "Other" ] ]
  ]

next :: Msg -> Model -> (Msg -> Effect Unit) -> Effect Unit
next msg mod h = mempty

update :: Msg -> Model -> Model
update msg model = case msg of
  Other -> model { message = "Other event fired in parent" }
  IncrementMsg m -> model { increment = Increment.update m model.increment }

init :: Model
init = { message: "", increment: Increment.init }

app :: App Msg Model
app = createApp { init, view, update, next }

main :: Effect Unit
main = do
  rootNode <- runApp app Nothing
  container <- getElementById "app"
  appendChildNode container rootNode
