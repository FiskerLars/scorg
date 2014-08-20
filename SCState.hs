{-# LANGUAGE ScopedTypeVariables,
OverloadedStrings, Rank2Types #-}

module SCState (SCState) where

import Data.RDF as R

data SCState = SCState
               { graph :: RDF r => r
               }