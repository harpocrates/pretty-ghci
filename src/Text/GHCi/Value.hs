{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.GHCi.Value where

import Text.GHCi.Value.Parser

-- base
import Data.String       ( fromString )
import Control.Exception ( catch, ErrorCall )

-- prettyprinter, prettyprinter-ansi-terminal
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

-- | Given a Haddock-formatted docstring, format and print that docstring to
-- the terminal. If a structured value cannot be parsed out, this falls back on
-- 'print'.
prettyPrintValue :: Show a => a -> IO ()
prettyPrintValue x = putDoc (value2Doc (show x)) `catch` \(_ :: ErrorCall) -> print x

-- | Parse a shown value into a pretty 'Doc'. Can throw an error on outputs
-- that could not be parsed properly, but should not throw errors for inputs
-- which are the outputs of 'show' from derived 'Show' instances.
value2Doc :: String -> Doc AnsiStyle
value2Doc shown = renderValue defaultValueConf value <> hardline
  where
    value = parseValue shown


-- | A Good Enough colour scheme
defaultValueConf :: ValuePrintConf
defaultValueConf = ValuePrintConf
  { vpc_number = color Magenta 
  , vpc_character = color Blue
  , vpc_string = color Green
  , vpc_control = bold <> colorDull Magenta
  , vpc_comma = color Yellow
  , vpc_operator = color White
  }

-- | Options for how to colour the terminal output
data ValuePrintConf = ValuePrintConf
  { vpc_number :: AnsiStyle    -- ^ all sorts of numeric literals
  , vpc_character :: AnsiStyle -- ^ character literals
  , vpc_string :: AnsiStyle    -- ^ string literals
  , vpc_control :: AnsiStyle   -- ^ various control characters (ex: parens)
  , vpc_comma :: AnsiStyle     -- ^ commas
  , vpc_operator :: AnsiStyle  -- ^ general operators
  }


-- | Function for turning a 'Value' into a 'Doc'
renderValue :: ValuePrintConf -> Value -> Doc AnsiStyle
renderValue vpc = renderVal
  where
    renderVal v = case v of
      Skip -> mempty

      Num n -> num (fromString n)
      Char c -> char (fromString c)
      Str s -> string (fromString s)
      
      List vs  -> renderSeq (ctrl "[") (map (align . renderVal) vs) (ctrl "]")
      Tuple vs -> renderSeq (ctrl "(") (map (align . renderVal) vs) (ctrl ")")
      
      -- Either everything goes on one line or the constructor and args each
      -- start on a new line (with args indented)
      Prefix c [] -> fromString c
      Prefix c vs ->
        let args = align (vsep (map (align . renderVal) vs))
        in fromString c <> group (nest 2 (line <> args))
     
      -- Either everything goes on one line, or each argument gets its own
      -- line with operators at the beginning of the lines
      Infix arg0 ops ->
        let tails = align (vsep (map (\(op,arg) -> optr (fromString op) <+> align (renderVal arg)) ops))
        in renderVal arg0 <> group (nest 2 (line <> tails))

      -- Either everything goes on one line or the constructor and fields each
      -- start on a new line (with fields indented)
      Record c vs ->
        let fields = align (vcat (zipWith (\l (f,x) -> l <+> fromString f <+> ctrl "=" <+> align (renderVal x))
                                          (ctrl "{" : repeat (ctrl ",")) vs))
        in fromString c <> group (nest 2 (line <> fields <+> ctrl "}"))
      
      Paren x -> ctrl "(" <> align (renderVal x) <> ctrl ")"

    -- Haskell style formatting of sequence-like things, with the comma at the
    -- start of the line
    renderSeq :: Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle -> Doc AnsiStyle
    renderSeq opn [] cls = opn <> cls
    renderSeq opn vs cls = align . group . encloseSep opn' cls' (coma ", ") $ vs
      where
        opn' = flatAlt (opn <> space) opn
        cls' = flatAlt (space <> cls) cls

    -- Useful annotations
    num    = annotate (vpc_number vpc)
    char   = annotate (vpc_character vpc)
    string = annotate (vpc_string vpc)
    ctrl   = annotate (vpc_control vpc)
    coma   = annotate (vpc_comma vpc)
    optr   = annotate (vpc_operator vpc)


