{
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -w #-}
module Language.Scheme.Scan (scan, Token(..)) where

import Language.Scheme.Number
import Data.Char
import Language.Scheme.Base
import qualified Data.Text as T

}

%wrapper "basic"

$hex_digit = [0-9 A-F a-f]
$letter = [A-Z a-z]
$digit = [0-9]

-- string chars
@str_char = [^ \\ \"] | "\" (. | \n)

tokens :-

-- whitespace
" " ;
[\n \r \t] ;
";" [^ \n \r]* ;

-- numbers
[0-9]+ { \str -> Literal $ Num $ fromInteger $ read str }

-- strings
\" @str_char* \" { \str -> Literal $ PureString $ T.pack $ read str }

-- characters
"#\" [.\n]    { \str -> char $ str !! 2 }
"#\nul"       { \_ -> char '\NUL' }
"#\alarm"     { \_ -> char '\a' }
"#\backspace" { \_ -> char '\b' }
"#\tab"       { \_ -> char '\t' }
"#\linefeed"  { \_ -> char '\n' }
"#\newline"   { \_ -> char '\n' }
"#\vtab"      { \_ -> char '\v' }
"#\page"      { \_ -> char '\f' }
"#\return"    { \_ -> char '\r' }
"#\esc"       { \_ -> char '\ESC' }
"#\space"     { \_ -> char ' ' }
"#\delete"    { \_ -> char '\DEL' }
"#\x" ($hex_digit)+ { \str -> char $ toEnum $ read $ '0' : drop 2 str }

-- booleans
"#" [Tt] { \_ -> Literal $ Bool True }
"#" [Ff] { \_ -> Literal $ Bool False }

-- identifiers
$letter ($letter | $digit)* "?"? { ident }
"+" { ident }
"-" { ident }
"*" { ident }
"/" { ident }

-- etc.
"(" { \_ -> LParen }
")" { \_ -> RParen }
"'" { \_ -> Tick }
"." { \_ -> Dot }

{

data Token
  = Literal Pure
  | LParen
  | RParen
  | Tick
  | Dot
  deriving (Eq, Show, Read)

scan :: String -> [Token]
scan = alexScanTokens

ident :: String -> Token
ident = Literal . PureSymbol

char :: Char -> Token
char = Literal . Char

}
