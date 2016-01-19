module Options where

import LanguageSelector

data Options = Options
     { inputGraphs :: [String]
     , inputBibTex :: [String]
     , language:: Language
     , outputType:: String } deriving Show

{-
instance Observable Options where
  observer = observeBase
-}
