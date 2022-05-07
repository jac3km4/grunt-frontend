module Component.View.Layout where

import Grunt.Component.Utils (css)
import Halogen.HTML as HH

type Props i a
  = { body :: Array (HH.HTML i a)
    , menu :: Array (HH.HTML i a)
    }

view :: forall i a. Props i a -> HH.HTML i a
view { body, menu } =
  HH.div_
    [ renderNav
    , HH.div [ css "page-body" ] body
    ]
  where
  renderNav =
    HH.nav_
      [ HH.ul [ css "nav-left" ] menu
      , HH.ul_ [ HH.strong_ [ HH.text "Grunt" ] ]
      , HH.ul [ css "nav-right" ] []
      ]
