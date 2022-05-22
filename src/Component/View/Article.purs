module Grunt.Component.View.Article
  ( view
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Grunt.Component.Utils (css)
import Grunt.Types (Entry(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Html.Renderer.Halogen as PH

view :: forall i a. Entry -> HH.HTML i a
view (Entry entry) =
  HH.div [ css "article-content" ]
    [ HH.small_ [ HH.text $ unwrap entry.published ]
    , HH.h2_ [ HH.text $ fold entry.title ]
    , HH.h4_ [ HH.text $ fold entry.author ]
    , HH.div_ [ content ]
    ]
  where
  rendered html = case PH.render_ html of
    Left err -> HH.text $ "This RSS entry contains invalid HTML: " <> err
    Right body -> body

  content = case entry.content of
    Just body
      | String.length body > 0 -> rendered body
      | otherwise -> fallbackContent
    Nothing -> fallbackContent

  fallbackContent =
    HH.div_
      [ maybe (HH.div_ []) (\src -> HH.img [ HP.src src ]) entry.imageUrl
      , rendered $ fold entry.summary
      , HH.div_ [ HH.a [ HP.href entry.url, HP.target "_blank" ] [ HH.text "See article" ] ]
      ]
