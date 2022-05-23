module Grunt.Component.View.FeedList
  ( Props
  , view
  ) where

import Prelude
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (uncurry)
import Grunt.Component.Utils (css)
import Grunt.Types (FeedId, Subscription(..), Tag(..), TaggingId)
import Halogen.HTML (AttrName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Safe.Coerce (coerce)

type Props i a
  = { feeds :: Array Subscription
    , tags :: Map Tag (Map FeedId TaggingId)
    , popupIsOpen :: Boolean
    , onAddSubscription :: a
    , onDeleteSubscription :: FeedId -> a
    , onToggleTagPopup :: a
    , onSubmitTagPopup :: a
    , onTagFeed :: Tag -> FeedId -> a
    , onUntagFeed :: Tag -> TaggingId -> a
    , feedUrlInput :: HH.HTML i a
    , tagInput :: HH.HTML i a
    }

view :: forall i a. Props i a -> HH.HTML i a
view props =
  HH.div [ css "side-bar side-bar-list" ]
    [ HH.div [ css "feed-list" ] (map renderSub props.feeds)
    , HH.div
        [ css "create-feed-form" ]
        [ props.feedUrlInput
        , HH.a [ HPA.role "button", HE.onClick $ const props.onAddSubscription ] [ HH.text "+" ]
        ]
    ]
  where
  renderSub sub@(Subscription { feedUrl, title, feedId }) =
    HH.details [ css "feed-list-item" ]
      [ HH.summary [] [ HH.text title ]
      , HH.div_
          [ HH.small [ css "feed-url" ] [ HH.text feedUrl ]
          , HH.fieldset [ css "feed-tags" ]
              $ Array.snoc (map (uncurry (renderTag sub)) (Map.toUnfoldable props.tags)) renderAddTag
          , HH.a
              [ css "delete-feed-button"
              , HPA.role "button"
              , HE.onClick $ const $ props.onDeleteSubscription feedId
              ]
              [ HH.text "Delete" ]
          ]
      , renderDialog
      ]

  renderTag (Subscription { feedId }) tag feeds =
    let
      entry = Map.lookup feedId feeds

      action = case entry of
        Just taggingId -> props.onUntagFeed tag taggingId
        Nothing -> props.onTagFeed tag feedId
    in
      HH.label_
        [ HH.input
            [ HP.type_ InputCheckbox
            , HP.checked $ isJust entry
            , HE.onClick $ const action
            ]
        , HH.text $ coerce tag
        ]

  renderAddTag =
    HH.span [ css "add-tag" ]
      [ HH.i [ css "fa fa-square-plus", HE.onClick $ const props.onToggleTagPopup ] []
      , HH.text "Add tag"
      ]

  renderDialog
    | props.popupIsOpen =
      HH.dialog [ HP.attr (AttrName "open") "" ]
        [ HH.article_
            [ HH.a [ css "close", HE.onClick $ const props.onToggleTagPopup ] []
            , HH.h3_ [ HH.text "Add a tag" ]
            , HH.div_ [ props.tagInput ]
            , HH.footer_
                [ HH.a [ HP.href "#", HPA.role "button", E.onClick $ const props.onSubmitTagPopup ] [ HH.text "Confirm" ]
                ]
            ]
        ]
    | otherwise = HH.div_ []
