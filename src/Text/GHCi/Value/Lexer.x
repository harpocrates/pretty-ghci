{
module Text.GHCi.Value.Lexer (
  lexTokens,
  Token(..),
) where

import Control.Monad ( join )
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

    $white+        ;
    
    "("            { const [OpenParen] }
    ")"            { const [CloseParen] }
    
    "["            { const [OpenBracket] }
    "]"            { const [CloseBracket] }
    
    "{"            { const [OpenBrace] }
    "}"            { const [CloseBrace] }
    
    ","            { const [Comma] }
    "="            { const [Equal] }
   
    @decimal       { pure . NumberTok }
    @number        { pure . NumberTok }
    @character     { pure . CharacterTok }
    @string        { pure . StringTok }
    
    @sym_op        { operatorsAndSymbols }

{

-- | Turn a 'String' into a list of 'Token'. This may error out for
-- particularly bad inputs (ex: unclosed string).
lexTokens :: String -> [Token]
lexTokens = join . alexScanTokens

-- | Our somewhat simplified version of GHC Haskell tokens 
data Token
  = NumberTok     String
  | StringTok     String
  | CharacterTok  String
  | OperatorTok   String
  | IdentifierTok String
  | OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | OpenBrace
  | CloseBrace
  | Comma
  | Equal
  deriving (Eq, Show)

-- | Split a string into a list of operators and identifiers
operatorsAndSymbols :: String -> [Token]
operatorsAndSymbols "" = []
operatorsAndSymbols (c:str)
  | isOperatorLike c
  , (op, rest) <- span isOperatorLike str
  = OperatorTok (c:op) : operatorsAndSymbols rest
  | otherwise
  , (ident, rest) <- span (not . isOperatorLike) str
  = case rest of
      '#':rest' -> IdentifierTok (c : ident ++ "#") : operatorsAndSymbols rest'
      _         -> IdentifierTok (c : ident       ) : operatorsAndSymbols rest

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
