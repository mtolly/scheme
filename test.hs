module Test where

import Language.Scheme.Scan
import Language.Scheme.Parse
import Language.Scheme.Base
import Language.Scheme.Prelude

interpret :: String -> Either String Pure
interpret = test . parse . scan

test :: Pure -> Either String Pure
test x = runScheme baseContext $ impurify x >>= eval >>= purify
