let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "debug"
              , "effect"
              , "quickcheck"
              , "assert"
              , "newtype"
              , "quickcheck-laws"
              , "arrays"
              , "console"
              , "foldable-traversable"
              ]
        }
