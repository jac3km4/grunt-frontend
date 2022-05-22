let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220516/packages.dhall
        sha256:b0bf932de16a10b7d69c6bbbb31ec9ca575237c43a999fa32e59e35eb8c024a1

in  upstream
  with html-parser-halogen =
    { dependencies =
      [ "arrays", "control", "dom-indexed", "halogen", "maybe", "prelude" ]
    , repo = "https://github.com/jac3km4/purescript-html-parser-halogen.git"
    , version = "purs-update-and-validation"
    }
