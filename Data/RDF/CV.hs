{-# LANGUAGE RankNTypes #-}
{-| Definition of a list of triplets/ subjects / relations that comprise the elements of a CV.
-}
module Data.RDF.CV where
import Data.List
import Data.RDF as R
import qualified Data.Text as T
import RdfHandler
import Data.RDF.Bibliography -- for academicTermOf (that is wrong there)

import Data.RDF.NS.Teach
import Data.RDF.NS.Foaf
import AcademicTerm

-- import Debug.Hood.Observe


cvParts:: forall a.RDF a => a -> R.Subject -> [[R.Node]]
cvParts g s = map (\f -> f g s ) [ cvPicture,
                                   cvNamePart
                                 , cvContactInfo ]

type CvPart = forall a. RDF a => a -> R.Subject -> [R.Node]
{-| Picture should return a list of objects that are Unodes
-}
cvPicture g s = union
                (queryObjects g s (mkUnode' foaf "img"))
                (queryObjects g s (mkUnode' foaf "depiction"))


{-| Returns literal nodes, first contains the name, following contain titles
-}
cvNamePart:: RDF a => a -> R.Subject -> [R.Node]
cvNamePart g s= foldr (\f l -> (f g s) ++ l) [] [ cvName 
                                                , cvTitle ]


cvMemberships:: RDF a => a -> R.Subject -> [R.Node]
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



{-| Example List of Publications.
TODO: fill up with my publications.
TODO: read from generic config/RDF (better use RDF-graph to determine them)
-}
cvPublications :: R.RDF a => a -> R.Subject -> [R.Subject]
cvPublications g s = map ((checknode g).publicationNode)
                     [ "fischer06:_secur_revoc_anony_authen_inter"
                     , "Doetzer2005a"
                     -- , "Fischer2010"
                     , "Fischer2008"
                     , "fischer08:measuringunlinkabilityrevisited"
                     , "Stumpf2007"
                     , "Fischer14PaymentSystemsDistributed"
                     , "Fischer14ProbabilisticPointProcesses"
                     , "Fisch2012MeasuringUnlinkabilityPrivacy"
                     , "Fischer03ProtectingIntegrityand"
                     , "FD14DesignandImplementation"
                     , "FischDK2011LinkGlobally-"
                     , "FHB+12EnhancingPrivacyin"
                     , "Fischer2011"
                     , "FHH13IndoorPositioningby"
                     , "HFB+2Contextawaretrust"
                     , "Karatas2014"
                     , "WesteFPK201150BucksAttack"]
  where
    checknode g n = case R.rdfContainsNode g n of
      True -> n
      False -> error $ "node " ++ (R.view n) ++ " does not exist in graph"











                              
