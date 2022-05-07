module Grunt.Component.View.FeedEntryList
  ( Props
  , view
  ) where

import Prelude
import Data.Foldable (fold)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Grunt.Component.Utils (css)
import Grunt.Types (Entry(..), EntryId, FeedId)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Props a
  = { entries :: Array Entry
    , reachedEnd :: Boolean
    , isUnread :: EntryId -> Boolean
    , isStarred :: EntryId -> Boolean
    , feedName :: FeedId -> Maybe String
    , onClick :: Entry -> a
    , onStar :: EntryId -> a
    , onUnstar :: EntryId -> a
    , onNextPage :: a
    }

view :: forall i a. Props a -> HH.HTML i a
view props =
  HH.div [ css "side-bar-list" ]
    [ HH.ul_ $ map (renderEntry props) props.entries
    , if not props.reachedEnd then
        HH.button
          [ css "secondary load-more-btn"
          , HE.onClick $ const props.onNextPage
          ]
          [ HH.text "Load more" ]
      else
        HH.div_ []
    ]
  where
  renderEntry { isUnread, isStarred, feedName, onClick, onStar, onUnstar } entry =
    let
      Entry { id, title, published, feedId } = entry
    in
      HH.li [ HE.onClick $ const $ onClick entry ]
        [ HH.div [ css $ if isUnread id then "entry-unread" else "entry-read" ] [ HH.text $ fold title ]
        , HH.div_ [ HH.small_ [ HH.text $ fold $ feedName feedId ] ]
        , HH.div_
            [ HH.small_ [ HH.text $ unwrap published ]
            , if isStarred id then
                HH.i [ css "fa-solid fa-star entry-star", HE.onClick $ const $ onUnstar id ] []
              else
                HH.i [ css "fa-regular fa-star entry-star", HE.onClick $ const $ onStar id ] []
            ]
        ]
