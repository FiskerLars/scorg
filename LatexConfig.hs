{-# LANGUAGE Rank2Types #-}

module LatexConfig where

import qualified Data.RDF as R
import qualified Data.RDF.Namespace as N
import qualified Data.Text as T 
import Data.List

import RdfHandler
import LanguageSelector

import Data.RDF.NS.Teach


data LatexConfig = LatexConfig { lang:: Language
                               }


defaultLatexConfig :: LatexConfig
defaultLatexConfig = LatexConfig { lang = EN }


allCourses:: R.RDF a => a -> [R.Subject]
allCourses g = map R.subjectOf
              $ R.query g
              Nothing
              (Just (mkUnode' teach "teacher"))
              (Just (R.unode $ T.pack "http://larsipulami.de/lars_fischer#me"))

