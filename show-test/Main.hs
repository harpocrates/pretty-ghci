module Main where

import Text.GHCi.Value
import Data.Text.Prettyprint.Doc
import System.Directory (listDirectory, createDirectoryIfMissing, findExecutable)
import Data.Foldable (for_)
import System.FilePath ((</>), takeFileName)
import System.Process (callProcess)
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import System.Console.GetOpt
import System.Environment (getArgs)
import Data.Maybe (catMaybes)

-- | Option parser
options :: [OptDescr Opt]
options = [ Option []    ["accept"] (NoArg Accept) "accept the output"
          , Option ['h'] ["help"]   (NoArg Help)   "display usage info"
          ]

-- | Options
data Opt = Accept | Help deriving Eq

main = do
  
  -- Command line args: do we want to accept or not?
  args <- getArgs
  let header = "Usage: show-test [OPTION...]"
  accept <- case getOpt Permute options args of
    (_, _, errs @ (_:_)) -> ioError (userError (concat errs                          ++ usageInfo header options))
    (_, non @ (_:_), []) -> ioError (userError ("Unexpected options: " ++ concat non ++ usageInfo header options))
    (opts, [], [])
      | Help `elem` opts -> ioError (userError (                                        usageInfo header options)) 
      | Accept `elem` opts -> pure True
      | otherwise -> pure False

  -- Get the test cases
  srcs <- listDirectory ("show-test" </> "src")
  createDirectoryIfMissing True ("show-test" </> "out")
  createDirectoryIfMissing True ("show-test" </> "ref")
  
  -- Run them in a loop
  for_ srcs $ \srcFile -> do
    putStrLn $ "Checking " ++ srcFile ++ "..."

    let ref = "show-test" </> "ref" </> srcFile
        out = "show-test" </> "out" </> srcFile
        src = "show-test" </> "src" </> srcFile

    inp <- readFile src
    let output = renderString (layoutPretty defaultLayoutOptions (value2Doc inp))
  
    -- Accept or test
    if accept
      then do writeFile ref output
      else do writeFile out output
              diff : _ <- catMaybes <$> traverse findExecutable ["colordiff", "diff"]
              callProcess diff [ref, out]
 
  -- Report status
  putStrLn $ "All " <> show (length srcs) <> " test cases " <> if accept then "accepted." else "passed."

