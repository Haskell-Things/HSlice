let haskellCi = https://raw.githubusercontent.com/fisx/github-actions-dhall/cabal-project-file/haskell-ci.dhall
                -- https://github.com/sorki/github-actions-dhall/pull/2

in    haskellCi.generalCi
        haskellCi.matrixSteps
        ( Some
            { ghc =
              [ haskellCi.GHC.GHC8104
              , haskellCi.GHC.GHC884
              ]
            , cabal = [ haskellCi.Cabal.Cabal32 ]
            }
        )
        // { on = [ haskellCi.Event.push
                  , haskellCi.Event.pull_request ]
           }
    : haskellCi.CI.Type
