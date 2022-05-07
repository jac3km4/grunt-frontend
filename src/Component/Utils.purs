module Grunt.Component.Utils
  ( css
  )
  where

import Prelude
import Halogen.HTML (ClassName(..), IProp)
import Halogen.HTML.Properties as HP

css :: forall r i. String -> IProp ( class :: String | r ) i
css = HP.class_ <<< ClassName
