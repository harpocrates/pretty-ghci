
import Data.Version               ( showVersion )
import System.Environment         ( getArgs, getProgName )
import System.Exit                ( die, exitSuccess )
import System.Console.GetOpt

import Text.PrettyPrint.GHCi      ( prettyPrintHaddock, prettyPrintValue )
import Paths_pretty_ghci          ( version )

main :: IO ()
main = do
  argv <- getArgs
  usage <- getUsage
  let exitBad msg = die (msg ++ usage)
      exitGood msg = putStr msg >> exitSuccess

  case getOpt Permute options argv of
    (_  , _, errs @ (_ : _)) -> exitBad (concat errs)
    (_  , args @ (_ : _), _) -> exitBad ("Unexpected arguments: " ++ unwords args)
    (opts, [], [])           -> do
      -- What to do with input?
      processInput <- case foldl (flip id) defaultOptions opts of
        Options { wantHelp = True } -> exitGood usage
        Options { wantVersion = True } -> exitGood versionMsg
        Options { inputType = it, smarterLayout = sl } -> case it of
          Nothing    -> exitBad "Specify either `--doc' or `--value'\n"
          Just Doc   -> pure (prettyPrintHaddock sl)
          Just Value -> pure (prettyPrintValue sl)

      -- Do that to the input
      str <- getContents
      processInput str

-- | Type of options to expect
data Options = Options
  { inputType :: Maybe InputType  -- ^ what input type to use
  , smarterLayout :: Bool         -- ^ use a smarter layout algorithm?
  , wantHelp :: Bool              -- ^ show the help
  , wantVersion :: Bool           -- ^ show the version
  }

defaultOptions :: Options
defaultOptions = Options Nothing False False False

-- | What sort of input to expect
data InputType = Doc | Value

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["doc"]            (NoArg (\o -> o{ inputType = Just Doc }))
           "pretty print the input assuming it is a docstring"
  , Option [] ["value"]          (NoArg (\o -> o{ inputType = Just Value }))
           "pretty print the input assuming it is a Haskell value"
  , Option [] ["smarter-layout"] (NoArg (\o -> o{ smarterLayout = True }))
           "use a smarter (but potentially slower) layout algorithm"
  , Option ['h'] ["help"]        (NoArg (\o -> o{ wantHelp = True }))
           "print out this usage info"
  , Option ['v'] ["version"]     (NoArg (\o -> o{ wantVersion = True }))
           "print out the version"
  ]

-- | Return usage information
getUsage :: IO String
getUsage = do
  prog <- getProgName
  let header = "Usage: " ++ prog ++ " [OPTION...]"
  pure (usageInfo header options)

-- | Message you get with the version
versionMsg :: String
versionMsg = "`ppghci' version " ++ ver ++ ", (c) Alec Theriault 2019\n"
  where ver = showVersion version

