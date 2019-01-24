### `pretty-ghci` [![Build Status][0]][1]

This library will make your GHCi experience colourful in 3 steps:

  1. Install the `pretty-ghci` library globally (`cabal new-install --global --lib .` from within
     the cloned repo or `cabal new-install --global --lib pretty-ghci` from Hackage)

  2. Modify your `~/.ghc/ghci.conf`

     ```haskell
     :set prompt      "λ> "
     :set prompt-cont "|> "

     import qualified Text.PrettyPrint.GHCi.prettyPrintValue
     pprint = Text.PrettyPrint.GHCi.prettyPrintValue False

     -- Typing `:pretty` will turn on the pretty-printing
     :def pretty \_ -> pure (":set -interactive-print pprint")
    
     -- Typing `:no-pretty` will turn off the pretty-printing
     :def no-pretty \_ -> pure (":set -interactive-print System.IO.print")

     -- Make things pretty by default!
     :pretty

     ```

  3. Enjoy!

[0]: https://travis-ci.org/harpocrates/pretty-ghci.svg?branch=master
[1]: https://travis-ci.org/harpocrates/pretty-ghci
