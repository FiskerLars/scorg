module LanguageSelector where

import Data.Char

data Language = DE | EN | LANG_DEFAULT | LANG_UNDEF deriving (Eq, Show)

{-| Todo Documentation
take the first two letters of a string to determine whether it means english or german.
|-}
readsLanguage:: String -> [(Language, String)] 
readsLanguage s = case (\(a,b) -> (map toLower a, b)) (splitAt 2 s) of
  ("de", r) -> [(DE, r)]
  ("en", r) -> [(EN, r)]
  (_, _) -> error "language not detected"

instance Read Language where
  readsPrec _ = readsLanguage

