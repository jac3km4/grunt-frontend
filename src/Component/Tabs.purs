module Grunt.Component.Tabs where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Grunt.Component.Utils (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as HPA

type State t
  = { tabs :: NonEmptyArray t, currentTab :: t }

data Query t a
  = SelectTab t a

data Action t
  = SelectedTab t

component :: forall t m. Eq t => Show t => H.Component (Query t) (NonEmptyArray t) t m
component =
  H.mkComponent
    { initialState: \tabs -> { tabs, currentTab: NEA.head tabs }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }
  where
  render { tabs, currentTab } =
    HH.li
      [ css "tab-group" ]
      $ NEA.toArray
      $ map (renderTab currentTab) tabs

  renderTab current tab =
    HH.a
      [ HPA.role "button"
      , css $ if current == tab then "tab-active" else "primary"
      , HE.onClick $ const $ SelectedTab tab
      ]
      [ HH.text $ show tab ]

  handleAction = case _ of
    SelectedTab tab -> do
      H.modify_ $ _ { currentTab = tab }
      H.raise tab

  handleQuery :: forall action o a. Query t a -> H.HalogenM (State t) action () o m (Maybe a)  
  handleQuery = case _ of
    SelectTab tab a -> do
      H.modify_ $ _ { currentTab = tab }
      pure $ Just a
