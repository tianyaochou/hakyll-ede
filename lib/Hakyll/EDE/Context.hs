module Hakyll.EDE.Context(
  EdeContext(..)
) where

import Hakyll
import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Text as ST
import qualified Text.EDE as E
import qualified Text.EDE.Filters as E

type DataMap = M.HashMap ST.Text Value
type FilterMap = M.HashMap E.Id E.Term
newtype EdeContext a = EdeContext { unEdeContext :: Item a -> Compiler (DataMap, FilterMap) }

edeBodyField :: String -> EdeContext String
edeBodyField k = EdeContext (\item -> return (M.fromList [(ST.pack k, String $ ST.pack $ itemBody item)], mempty))
