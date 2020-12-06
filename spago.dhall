{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "chord-editor"
, dependencies =
  [ "console"
  , "drawing"
  , "effect"
  , "halogen-components"
  , "routing"
  , "routing-duplex"
  , "simple-json"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
