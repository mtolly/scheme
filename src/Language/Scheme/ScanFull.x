{
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -w #-}
module Language.Scheme.Scan (scan, Token(..)) where

import Language.Scheme.Number
import Data.Char

}

%wrapper "basic"

$hex_digit = [0-9 A-F a-f]
$letter = [A-Z a-z]
$digit = [0-9]

-- identifier pieces
$special_initial = [\!\$\%\&\*\/\:\<\=\>\?\^\_\~]
$constituent = $letter -- needs unicode
@inline_hex_esc = "\x" $hex_digit+ ";"
@initial = $constituent | $special_initial | @inline_hex_esc
$special_subsequent = [\+\-\.\@]
@subsequent = @initial | $digit | $special_subsequent -- needs unicode
@peculiar_ident = "+" | "-" | "..." | "->" @subsequent*

-- string chars
@str_char = [^ \\ \"] | "\\" (. | \n)

tokens :-

-- whitespace
" " ;
[\n \r \t] ;
";" [^ \n \r]* ;

-- numbers
[0-9]+ { \str -> Num $ fromInteger $ read str }

-- strings
\" @str_char* \" { \str -> String $ read str }

-- characters
"#\" [.\n]    { \str -> Char $ str !! 2 }
"#\nul"       { \_ -> Char '\NUL' }
"#\alarm"     { \_ -> Char '\a' }
"#\backspace" { \_ -> Char '\b' }
"#\tab"       { \_ -> Char '\t' }
"#\linefeed"  { \_ -> Char '\n' }
"#\newline"   { \_ -> Char '\n' }
"#\vtab"      { \_ -> Char '\v' }
"#\page"      { \_ -> Char '\f' }
"#\return"    { \_ -> Char '\r' }
"#\esc"       { \_ -> Char '\ESC' }
"#\space"     { \_ -> Char ' ' }
"#\delete"    { \_ -> Char '\DEL' }
"#\x" ($hex_digit)+ { \str -> Char $ toEnum $ read $ '0' : drop 2 str }

-- booleans
"#" [Tt] { \_ -> Bool True }
"#" [Ff] { \_ -> Bool False }

-- identifiers
@initial @subsequent* | @peculiar_ident
  { Ident . inlineHexEscape }

-- etc.
"(" { \_ -> LParen }
")" { \_ -> RParen }
"{" { \_ -> LBrace }
"}" { \_ -> RBrace }
"[" { \_ -> LBracket }
"]" { \_ -> RBracket }
"#(" { \_ -> HashParen }
"#vu8(" { \_ -> ByteParen }
"'" { \_ -> Tick }
"`" { \_ -> Backtick }
"," { \_ -> Comma }
",@" { \_ -> CommaAt }
"." { \_ -> Period }
"#'" { \_ -> HashTick }
"#`" { \_ -> HashBacktick }
"#," { \_ -> HashComma }
"#,@" { \_ -> HashCommaAt }

{

inlineHexEscape :: String -> String
inlineHexEscape "" = ""
inlineHexEscape ('\\':'x':rest) = case reads ("0x" ++ rest) of
  [(int, ';' : after)] -> toEnum int : inlineHexEscape after
  _ -> error "inlineHexEscape: fatal error"
inlineHexEscape (c:cs) = c : inlineHexEscape cs

data Token
  = Ident String
  | Bool Bool
  | Num SchemeComplex
  | Char Char
  | String String
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace -- reserved
  | RBrace -- reserved
  | HashParen
  | ByteParen
  | Tick
  | Backtick
  | Comma
  | CommaAt
  | Period
  | HashTick
  | HashBacktick
  | HashComma
  | HashCommaAt
  deriving (Eq, Show, Read)

scan :: String -> [Token]
scan = alexScanTokens

}
