module Main (main) where

import Oak
import Increment as Increment
import Oak.Debug (debugApp)
import Data.Array ((..))
import Control.MonadPlus (guard)
import Data.Int (even)
import Fetch as F
import Effect.Aff
import Data.Functor (void)
import Data.Either (either)

import Prelude
  ( Unit
  , (>>=)
  , bind
  , discard
  , ($)
  , (*)
  , (-)
  , (+)
  , pure
  , class Show
  , (<>)
  , bind
  , map
  , mempty
  , show
  )
import Effect
import Effect.Console

type Model =
  { increment :: Increment.Model
  , message :: String
  }

data Msg
  = IncrementMsg Increment.Msg
  | SetText String
  | GoGet

instance showMsg :: Show Msg where
  show (IncrementMsg m) = "Subapp " <> show m
  show (SetText s) = "SetText " <> s
  show GoGet = "GoGet"

view :: Model -> Html Msg
view model = div []
  [ text model.message
  , (map IncrementMsg (Increment.view model.increment))
  , text "this is the parent app"
  , div [] [ button [ onClick GoGet ] [ text "Perform GET" ] ]
  , div [] do
      x <- 1 .. 10
      guard (even x)
      pure $ div [] [ text (show x) ]
  ]

handle :: (Msg -> Effect Unit) -> Either Error String -> Effect Unit
handle run either = case either of
  Left e -> logShow e
  Right result -> run $ SetText $ "got " <> result

next :: Msg -> Model -> (Msg -> Effect Unit) -> Effect Unit
next GoGet mod run = runAff_ (handle run) do
  { url } <- F.fetch "https://httpbin.org/get" {}
  pure url
next _ _ _ = mempty

update :: Msg -> Model -> Model
update msg model = case msg of
  SetText s -> model { message = s }
  IncrementMsg m -> model { increment = Increment.update m model.increment }
  GoGet -> model { message = "performing request!" }

init :: Model
init = { message: "", increment: Increment.init }

app :: App Msg Model
app = createApp { init, view, update, next }

main :: Effect Unit
main = do
  rootNode <- runApp (debugApp app) Nothing
  container <- getElementById "app"
  appendChildNode container rootNode
