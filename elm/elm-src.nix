elm2nix 0.2.1

Usage: elm2nix COMMAND
  Convert Elm project to Nix expressions

Available options:
  -h,--help                Show this help text

Available commands:
  init                     Generate default.nix (printed to stdout)
  convert                  Generate Nix expressions for elm.json using
                           nix-prefetch-url
  snapshot                 Generate versions.dat



    Usage:

      $ elm2nix init > default.nix
      $ elm2nix convert > elm-srcs.nix
      $ nix-build

      Note: You have to run elm2nix from top-level directory of an Elm project.
    
