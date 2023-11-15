let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/main/haskell-ci.dhall

in    haskellCi.generalCi
        haskellCi.defaultCabalSteps
        haskellCi.DhallMatrix::{
        , ghc =
          [ haskellCi.GHC.GHC963, haskellCi.GHC.GHC947, haskellCi.GHC.GHC928 ]
        }
    : haskellCi.CI.Type
