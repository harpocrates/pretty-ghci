{
module Text.PrettyPrint.GHCi.Value.Parser (
  parseValue,
  Id, Op, Value(..),
) where

import Text.PrettyPrint.GHCi.Value.Lexer

import qualified Data.List.NonEmpty as N
}

%name parseTokens value
%monad { Maybe } { (>>=) } { return }
%expect 0
%tokentype { Token }
%token number     { NumberTok $$ }
       string     { StringTok $$ }
       character  { CharacterTok $$ }
       operator   { OperatorTok $$ }
       identifier { IdentifierTok $$ }
       '('        { OpenParen }
       ')'        { CloseParen }
       '['        { OpenBracket }
       ']'        { CloseBracket }
       '{'        { OpenBrace }
       '}'        { CloseBrace }
       ','        { Comma }
       '='        { Equal }

%%

atom :: { Value }
    : number                           { Num   $1 }
    | string                           { Str   $1 }
    | character                        { Char  $1 }
    | '(' ')'                          { Tuple [] }
    | '(' value comma_values ')'       { if null $3
                                           then Paren $2
                                           else Tuple ($2 : reverse $3) }
    | '[' ']'                          { List [] }
    | '[' value comma_values ']'       { List ($2 : reverse $3) }

-- Reversed list of values, each value being preceded by a comma
comma_values :: { [Value] }
    : {- empty -}                      { []      }
    | comma_values ',' value           { $3 : $1 }

-- Prefix constructor application
prefix :: { Value }
    : identifier prefix_apps           { Prefix $1 (reverse $2) }
    | identifier '{' fields '}'        { Record $1 (N.reverse $3) }
    | atom                             { $1 }

-- Reversed arguments to a prefix constructor
prefix_apps :: { [Value] }
    : {- empty -}                      { []                }
    | prefix_apps atom                 { $2           : $1 }
    | prefix_apps identifier           { Prefix $2 [] : $1 }

-- A record field
field :: { (Id, Value) }
    : identifier       '=' value       { ($1,               $3) }
    | '(' operator ')' '=' value       { ("(" ++ $2 ++ ")", $5) }

-- Non-empty list of reversed record fields
fields :: { N.NonEmpty (Id, Value) }
    : field                            { $1 N.:| [] }
    | fields ',' field                 { N.cons $3 $1 }

-- Infix constructor application
infixes :: { Value }
    : prefix infixes_sufs              { case $2 of
                                           [] -> $1
                                           x : xs -> Infix $1 (N.reverse (x N.:| xs)) }

-- Reversed list of operator suffixes
infixes_sufs :: { [(Op, Value)] }
    : {- empty -}                      { []            }
    | infixes_sufs operator prefix     { ($2, $3) : $1 }

-- Entry point
value :: { Value }
    : infixes                          { $1 }

{
-- | Throws an exception, not particularly helpful
happyError :: [Token] -> Maybe a
happyError _ = Nothing

-- | A @conid@ or @varid@ (possibly ending in hashes, to account for @MagicHash@)
type Id = String

-- | A @conop@ or @varop@
type Op = String

-- | A very simple representation of the output of 'Show'
data Value
  = Prefix Id [Value]
  | Infix Value (N.NonEmpty (Op, Value))
  | Record Id (N.NonEmpty (Id, Value))
  | Tuple [Value]
  | List [Value]
  | Num String  -- ^ integer or floating point
  | Char String -- ^ character
  | Str String  -- ^ string
  | Paren Value
  deriving Show

-- | Parse a value from a 'String'. Returns 'Nothing' for inputs that
-- could not be parsed.
parseValue :: String -> Maybe Value
parseValue = parseTokens . filter notWhite . lexTokens
  where
    notWhite (WhiteTok _) = False
    notWhite _ = True
}
