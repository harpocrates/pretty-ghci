### `pretty-ghci` [![Build Status][0]][1]

This library will make your GHCi experience colourful in 3 steps:

  1. Install the executable globally with `cabal v2-install pretty-ghci`

  2. Modify your `~/.ghc/ghci.conf`

     ```haskell
     :set prompt      "λ> "
     :set prompt-cont "|> "

     :{
     let pprint x = System.Process.withCreateProcess cp' $ \(Just i) _ _ ph -> do
             System.IO.hPutStrLn i (show x)
             System.IO.hClose i
             System.Process.waitForProcess ph
             pure ()
           where cp = System.Process.proc "pp-ghci" ["--value", "--smarter-layout"]
                 cp' = cp{ System.Process.std_out = System.Process.Inherit 
                         , System.Process.std_err = System.Process.Inherit
                         , System.Process.std_in  = System.Process.CreatePipe }
     :}

     -- Typing `:pretty` will turn on the pretty-printing
     :def pretty \_ -> pure (":set -interactive-print pprint")

     -- Typing `:no-pretty` will turn off the pretty-printing
     :def no-pretty \_ -> pure (":set -interactive-print System.IO.print")

     -- Make things pretty by default!
     :pretty
     ```

  3. Enjoy!

## Advantages over existing alternatives

  * One stop-solution for formatting and coloring with a small dependency graph
  * Takes your terminal width into account during the layout step
  * Works for values whose `Show` instance doesn't produce valid Haskell (ex: `Show (->)`)
  * Your output will be coloured even if parsing fails
  * Install one global executable, not one library per GHC version

[0]: https://travis-ci.org/harpocrates/pretty-ghci.svg?branch=master
[1]: https://travis-ci.org/harpocrates/pretty-ghci
