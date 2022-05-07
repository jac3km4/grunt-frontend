let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220503/packages.dhall
        sha256:847d49acea4803c3d42ef46114053561e91840e35ede29f0a8014d09d47cd8df

in  upstream
  with html-parser-halogen =
    { dependencies =
        [ "arrays"
        , "bifunctors"
        , "control"
        , "dom-indexed"
        , "either"
        , "lists"
        , "strings"
        , "halogen"
        , "prelude"
        , "string-parsers"
        ]
    , repo =
        "https://github.com/jac3km4/purescript-html-parser-halogen.git"
    , version =
        "purs-update"
    }
