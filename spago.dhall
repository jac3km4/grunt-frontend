{ name = "grunt-front"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "html-parser-halogen"
  , "http-methods"
  , "js-date"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "strings"
  , "transformers"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}