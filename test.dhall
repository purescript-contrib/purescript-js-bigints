let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "spec"
              , "debug"
              , "spec-discovery"
              , "quickcheck"
              , "assert"
              , "quickcheck-laws"
              , "arrays"
              , "console"
              , "foldable-traversable"
              ]
        }
