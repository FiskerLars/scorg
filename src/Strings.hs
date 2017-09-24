module Strings (replaceString) where

import Data.List

replaceString:: String -> String -> String -> String
replaceString old new str =
  mapSubstrAcc (\s@(x:xs) -> 
                 case stripPrefix old s of
                   (Just rem) -> (rem, (++) new) 
                   (Nothing) -> (xs, (++) [x])
               ) str 

mapSubstrAcc:: (String  -> (String, (String-> String))) -> String ->  String
mapSubstrAcc f str = foldr (\a b -> a b) "" $ substrApply f str
                  where
                    substrApply _ [] = [] 
                    substrApply f str =
                      let (rem, res)  = f str
                      in res : substrApply f rem
