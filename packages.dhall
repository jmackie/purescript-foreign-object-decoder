let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0/src/packages.dhall sha256:5ed5e0db195768ffee2e657698ec429fcf1790330241d24a7916d27fce73870c

let overrides = {=}

let additions = {=}

in  upstream ⫽ overrides ⫽ additions
