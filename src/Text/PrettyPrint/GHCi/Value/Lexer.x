{
module Text.PrettyPrint.GHCi.Value.Lexer (
  lexTokens,
  Token(..),
) where

import Data.Char     ( generalCategory, GeneralCategory(..) )
}

%wrapper "basic"

$binit     = 0-1
$octit     = 0-7
$decdigit  = 0-9
$hexit     = [$decdigit A-F a-f]

-- Number stuff
@numspc       = _*
@decimal      = $decdigit(@numspc $decdigit)*
@binary       = $binit(@numspc $binit)*
@octal        = $octit(@numspc $octit)*
@hexadecimal  = $hexit(@numspc $hexit)*
@exponent     = @numspc [eE] [\-\+]? @decimal
@bin_exponent = @numspc [pP] [\-\+]? @decimal

@floating_point = @numspc @decimal \. @decimal @exponent? | @numspc @decimal @exponent
@hex_floating_point = ( @numspc @hexadecimal \. @hexadecimal @bin_exponent?
                      | @numspc @hexadecimal @bin_exponent
                      )

@magic_hash  = ( \# | \#\# )?
@number_prim = ( @decimal
               | 0[bB] @numspc @binary
               | 0[oO] @numspc @octal
               | 0[xX] @numspc @hexadecimal
               | @floating_point
               | 0[xX] @numspc @hex_floating_point
               )
@number      = [\-]? @number_prim @magic_hash

-- Characters and strings literal
@char_escape = \\ ( [abfnrtv\\"']
                  | \^ [@-_]
                  | x $hexit+
                  | o $octit+
                  |   $decdigit+
                  | NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS  | HT
                  | LF  | VT  | FF  | CR  | SO  | SI  | DLE | DC1 | DC2 | DC3
                  | DC4 | NAK | SYN | ETB | CAN | EM | SUB | ESC  | FS  | GS
                  | RS  | US  | SP  | DEL
                  )

@character    = \' (~ [\\ \'] | \\ \' | @char_escape)  \'
@string       = \" (~ [\\ \"] | \\ \" | @char_escape)* \"

-- Symbol or operator (everything else)

$sym_op_char  = ~[ $white \[ \] \( \) \{ \} \, \" \' \= ]
@sym_op       = ($sym_op_char # $decdigit) $sym_op_char*

tokens :-

    $white+        { WhiteTok }

    "("            { const OpenParen }
    ")"            { const CloseParen }

    "["            { const OpenBracket }
    "]"            { const CloseBracket }

    "{"            { const OpenBrace }
    "}"            { const CloseBrace }

    ","            { const Comma }
    "="            { const Equal }

    @decimal       { NumberTok }
    @number        { NumberTok }
    @character     { CharacterTok }
    @string        { StringTok }

    @sym_op        { operatorOrSymbol }

{

-- | Turn a 'String' into a list of 'Token'. This may error out for
-- particularly bad inputs (ex: unclosed string).
lexTokens :: String -> [Token]
lexTokens = alexScanTokens

-- | Our somewhat simplified version of GHC Haskell tokens
data Token
  = WhiteTok      String
  | NumberTok     String
  | StringTok     String
  | CharacterTok  String
  | OperatorTok   String
  | IdentifierTok String  -- ^ we're overly liberal with what can be an
                          -- identifier (so as to accomodate custom 'Show'
                          -- instances)
  | OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | OpenBrace
  | CloseBrace
  | Comma
  | Equal
  deriving (Eq, Show)

-- | Classify a string as either an operator or an identifier
operatorOrSymbol :: String -> Token
operatorOrSymbol str
  | all isOperatorLike str = OperatorTok str
  | otherwise              = IdentifierTok str

-- | Characters in these categories can be part of operators
isOperatorLike :: Char -> Bool
isOperatorLike '_' = False
isOperatorLike c = generalCategory c `elem` [ ConnectorPunctuation
                                            , DashPunctuation
                                            , OtherPunctuation
                                            , MathSymbol
                                            , CurrencySymbol
                                            , ModifierSymbol
                                            , OtherSymbol ]

}
