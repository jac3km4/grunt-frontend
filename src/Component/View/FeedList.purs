module Grunt.Component.View.FeedList
  ( Props
  , view
  ) where

import Prelude
import Grunt.Component.Utils (css)
import Grunt.Types (FeedId, Subscription(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as HPA

type Props i a
  = { feeds :: Array Subscription
    , isHidden :: FeedId -> Boolean
    , onAddSubscription :: a
    , onDeleteSubscription :: FeedId -> a
    , onHideSubscription :: FeedId -> a
    , onUnhideSubscription :: FeedId -> a
    , input :: HH.HTML i a
    }

view :: forall i a. Props i a -> HH.HTML i a
view props =
  HH.div [ css "side-bar-list" ]
    [ HH.ul_ (map renderSub props.feeds)
    , HH.div
        [ css "create-feed-form" ]
        [ props.input
        , HH.a [ HPA.role "button", HE.onClick $ const props.onAddSubscription ] [ HH.text "+" ]
        ]
    ]
  where
  renderSub (Subscription { feedUrl, feedId }) =
    HH.li [ css "feed-list" ]
      [ HH.span_ [ HH.text feedUrl ]
      , HH.span [ css "feed-attrs" ]
          [ if props.isHidden feedId then
              HH.i [ css "fa fa-eye-slash", HE.onClick $ const $ props.onUnhideSubscription feedId ] []
            else
              HH.i [ css "fa fa-eye", HE.onClick $ const $ props.onHideSubscription feedId ] []
          , HH.i [ css "fa fa-trash", HE.onClick $ const $ props.onDeleteSubscription feedId ] []
          ]
      ]
