module Text.PrettyPrint.GHCi (
  -- * Interactive expression printing
  prettyPrintValue, value2Doc,
  ValuePrintConf(..),
  defaultValueConf,

  -- * Interactive doc string printing
  prettyPrintHaddock, haddock2Doc,
  HaddockPrintConf(..),
  defaultHaddockConf,

  -- * Formatting options
  AnsiStyle,
  -- ** Color
  color, colorDull, bgColor, bgColorDull, Color(..),
  -- ** Style
  bold, italicized, underlined,
) where

import Text.PrettyPrint.GHCi.Value
import Text.PrettyPrint.GHCi.Haddock

import Data.Text.Prettyprint.Doc.Render.Terminal

