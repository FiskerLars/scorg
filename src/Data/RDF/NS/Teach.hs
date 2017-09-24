module Data.RDF.NS.Teach where

import Data.List
import Data.Maybe


import qualified Data.RDF as R
import qualified Data.Text as T
import qualified Data.RDF.Namespace as N
import Text.Read (readMaybe)

-- scorg imports
import RdfHandler
import AcademicTerm

teach = N.mkPrefixedNS' "teach" "http://linkedscience.org/teach/ns#"


academicTermPred = mkUnode' teach  "academicTerm"


-- teaching experiences (event object -> academicTerm Obj)
academicTermObjs:: RDFdb -> R.Subject -> [R.Object]
academicTermObjs = objectsByPred academicTermPred  
academicTermObj:: RDFdb -> R.Subject -> Maybe (R.Object)
academicTermObj  g s = maybe
  (error $ "could not find academicTerm-property for "++ R.view s)
  (return)
  $ (listToMaybe $ academicTermObjs g s )


-- subject shoud be lecture/event
academicTerm:: RDFdb -> R.Subject -> AcademicTerm
academicTerm g s =  maybe
  (error $ "Could not parse AcademicTerm of " ++ ((R.view).fromJust) aterm)
  (id )
  $ aterm >>= (readMaybe.(nameOfNode g))  
  where
    aterm = academicTermObj g s

{-| Group a set of courses or workshops (see http://linkedscience.org/teach/ns) by academic Term.
|-}
groupByAcademicTerm:: RDFdb -> [R.Subject] -> [[R.Subject]]
groupByAcademicTerm g = groupBy (\s s' ->  (academicTermObj g s) == (academicTermObj g s'))




{- TODO separate autonomous teaching from supervised teaching from thesis advisor
-}
groupByTypeClasses:: RDFdb -> [R.Subject] -> [[R.Subject]]
groupByTypeClasses = undefined


sortTeachingEntries:: RDFdb -> [R.Subject] -> [R.Subject]
sortTeachingEntries g = sortBy (lectureOrdering g)


lectureOrdering g s s' = compare (academicTerm g s') (academicTerm g s) 
