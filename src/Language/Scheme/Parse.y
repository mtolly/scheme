{
module Language.Scheme.Parse (parse) where

import Language.Scheme.Base
import Language.Scheme.Scan
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  lit { Literal $$ }
  '(' { LParen }
  ')' { RParen }
  quote { Tick }
  '.' { Dot }

%%

Value : lit { $1 }
      | '(' Values ')' { $2 }
      | quote Value { PurePair (PureSymbol "quote") (PurePair $2 Null) }

Values : { Null }
       | Value Values { PurePair $1 $2 }
       | '.' Value { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
