module Data.RDF.NS.Teach where

import Data.List
import Data.Maybe


import qualified Data.RDF as R
import qualified Data.Text as T
import qualified Data.RDF.Namespace as N

-- scorg imports
import RdfHandler
import AcademicTerm

teach = N.mkPrefixedNS' "teach" "http://linkedscience.org/teach/ns#"


academicTermPred = mkUnode' teach  "academicTerm"
                   --R.unode $ T.pack "http://linkedscience.org/teach/ns#academicTerm"


-- teaching experiences
academicTermsOf:: R.RDF a => a -> R.Subject -> [R.Object]
academicTermsOf = objectsByPred academicTermPred  
academicTermOf:: R.RDF a => a -> R.Subject -> Maybe (R.Object)
academicTermOf  = (listToMaybe.).academicTermsOf 



{-| Group a set of courses or workshops (see http://linkedscience.org/teach/ns) by academic Term.
|-}
groupByAcademicTerm:: R.RDF a => a -> [R.Subject] -> [[R.Subject]]
groupByAcademicTerm g = groupBy (\s s' ->  (academicTermOf g s) == (academicTermOf g s'))


{- TODO separate autonomous teaching from supervised teaching from thesis advisor
-}
groupByTypeClasses:: R.RDF a => a -> [R.Subject] -> [[R.Subject]]
groupByTypeClasses = undefined

sortTeachingEntries:: R.RDF a => a -> [R.Subject] -> [R.Subject]
sortTeachingEntries g = sortBy lectureOrdering
                        where
                          lectureOrdering s s' = compare
                                                 (academicTermOf g s'  >>= return.readATobj)
                                                 (academicTermOf g s >>= return.readATobj) -- FIXME: order by termTODO: order Course,Lecture,.. (quality rang)
                            where
                              readATobj:: R.Node -> AcademicTerm
                              readATobj = read.tail.(R.view)
