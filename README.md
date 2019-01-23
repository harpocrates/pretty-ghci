# `pretty-ghci` [![Build Status][0]][1]

This library will make your GHCi experience colourful in 3 steps:

  1. Install the `pretty-ghci` library globally

  2. Modify your `~/.ghc/ghci.conf`

     ```haskell
     :set prompt      "λ> "
     :set prompt-cont "|> "

     -- Typing `:pretty` will turn on the pretty-printing
     :def pretty \_ -> pure ("let ipp = Text.PrettyPrint.GHCi.prettyPrintValue False\n:set -interactive-print ipp")
    
     -- Typing `:no-pretty` will turn off the pretty-printing
     :def no-pretty \_ -> pure (":set -interactive-print System.IO.print")

     -- Make things pretty by default!
     :pretty

     ```

  3. Enjoy!

[0]: https://travis-ci.org/harpocrates/pretty-ghci.svg?branch=master
[1]: https://travis-ci.org/harpocrates/pretty-ghci
