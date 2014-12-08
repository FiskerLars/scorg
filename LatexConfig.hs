
module LatexConfig where

import qualified Data.RDF as R
import qualified Data.RDF.Namespace as N
import Data.Text hiding (map)

import RdfBibliography

myCourses:: R.RDF a => a -> [R.Subject]
myCourses g = map R.subjectOf
              $ R.query g
              Nothing
              (Just (mkUnode' teach "teacher"))
              (Just (R.unode $ pack "http://larsipulami.de/lars_fischer#me"))

