{-# LANGUAGE RankNTypes #-}
{-| Definition of a list of triplets/ subjects / relations that comprise the elements of a CV.
-}
module Data.RDF.CV where
import Data.List
import Data.RDF as R
import qualified Data.Text as T
import RdfHandler -- most RDF-Shortcuts
import Data.RDF.Bibliography -- for academicTermOf (that is wrong there)

import Data.RDF.NS.Teach
import Data.RDF.NS.Foaf
import AcademicTerm

-- import Debug.Hood.Observe


cvParts:: RDFdb -> R.Subject -> [[R.Node]]
cvParts g s = map (\f -> f g s ) [ cvPicture,
                                   cvNamePart
                                 , cvContactInfo ]

type CvPart = RDFdb -> R.Subject -> [R.Node]
{-| Picture should return a list of objects that are Unodes
-}
cvPicture g s = union
                (queryObjects g s (mkUnode' foaf "img"))
                (queryObjects g s (mkUnode' foaf "depiction"))


cvPublications:: RDFdb -> R.Subject -> [R.Node]
cvPublications g s = union
  (queryObjects g s authorPred) 
  (queryObjects g subject_name authorPred)
  where
    subject_name = headNodeOrError ("Subject "
                                    ++ (view s)
                                    ++" in Publication search has no "
                                    ++ (view (mkUnode' foaf "name")) )
      $ queryObjects g s (mkUnode' foaf "name")


{-| Returns literal nodes, first contains the name, following contain titles
-}
cvNamePart:: RDFdb -> R.Subject -> [R.Node]
cvNamePart g s= foldr (\f l -> (f g s) ++ l) [] [ cvName 
                                                , cvTitle ]


cvMemberships:: RDFdb -> R.Subject -> [R.Node]
cvMemberships g s = map R.subjectOf $ R.query g Nothing (Just typePred) (Just s)

cvName  g s = union
              (queryObjects g s (mkUnode' foaf "name"))
              (queryObjects g s (mkUnode' dct "name"))
              
cvTitle g s = queryObjects g s (mkUnode' foaf "name")

              

cvContactInfo g s = map (\f -> f g s) [ cvAddress 
                                      , cvPhone
                                      , cvMailbox
                                      , cvHomepage ]

{-| Selects first home address to find, error if no address exists. |-}
cvAddress g s = headOrFailNode "vcard Address missing"
                $ foldr (\s' l -> queryObjects g s' (mkUnode' vcard "Home") ++ l) []
                $ objectsByPred (mkUnode' vcard "hasAddress") g s 

cvPhone g s = headOrFailNode "vcard Phone missing" 
              $ filter (\s' ->  [] /= queryObjects g s' (mkUnode' vcard "Cell"))
              $ filter (\s' ->  [] /= queryObjects g s' (mkUnode' vcard "Voice"))
              $ queryObjects g s (mkUnode' vcard "hasTelephone") -- vcard:phone

cvMailbox g s = headOrFailNode  "foaf:mbox missing" $ queryObjects g s (mkUnode' foaf "mbox")
cvHomepage g s = headOrFailNode "foaf:homepage missing" $ queryObjects g s (mkUnode' foaf "homepage") 














                              
