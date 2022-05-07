module Grunt.Component.TextInput
  ( Query(..)
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Grunt.Component.Utils (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = GetValue (String -> a)
  | SetError String a

data Action
  = ChangeValue String

type State
  = { value :: String, placeholder :: String, error :: Maybe String }

component :: forall o m. H.Component Query { placeholder :: String } o m
component =
  H.mkComponent
    { initialState: \{ placeholder } -> { value: "", placeholder, error: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery, handleAction = handleAction }
    }
  where
  render { value, placeholder, error } =
    HH.div_
      [ HH.input [ HP.type_ HP.InputText, HP.placeholder placeholder, HP.value value, HE.onValueInput ChangeValue ]
      , case error of
          Just msg -> HH.div [ css "error-message" ] [ HH.text msg ]
          Nothing -> HH.div_ []
      ]

  handleAction = case _ of
    ChangeValue value -> H.modify_ $ _ { value = value, error = Nothing }

  handleQuery :: forall action a. Query a -> H.HalogenM State action () o m (Maybe a)
  handleQuery = case _ of
    GetValue reply -> do
      { value } <- H.get
      pure $ Just $ reply value
    SetError error a -> do
      H.modify_ $ _ { error = Just error }
      pure $ Just a
