{-| Generate Latex Output from the graph. Currently implements my CV
-}
module LatexOutput (Latex,
                    genericPublicationList
                   , genericTeachingList
                   , genCvLatex)
       where

import Data.List
--import Data.String.Utils
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.RDF as R


import Debug.Hood.Observe


import RdfBibliography
import RdfHandler
import AcademicTerm

import LatexConfig
import RdfCv
import LanguageSelector
import Foaf

import Strings

type Latex = String

latexCommasAnd:: [Latex] -> Latex
latexCommasAnd [x]   = x
latexCommasAnd [x,y] = x ++ " and " ++ y
latexCommasAnd (x:xs) = x ++ ", " ++ latexCommasAnd xs
latexCommasAnd [] = ""

{-|
The following part utilises the ecv-plus latex class to generate a publication list from a given list of ordered BibTeX identifiers.

Example LaTeχ looks as:

\ecvSection{\ecvPublications{}}

\ecvTagPlainValueRagged{2013}{Fischer, L.; Hoffman, A.; Hahn, N.:\\
\emph{Indoor Positioning by Fusion of IEEE 802.11 Fingerprinting and Compass Bearing}\\
in Ortsbezogene Anwendungen und Dienste 9. Fachgespräch der GI/ITG-Fachgruppe Kommunikation und Verteilte Systeme, Universitätsverlag Chemnitz, 2013}\\

\ecvTagPlainValueRagged{2012}
{Fischer, L.; Heupel, M.; Bourimi, M. Kesdoǧan, D.; Gimenez, R.:
\emph{Enhancing Privacy in Collaborative Scenarios Utilising a Flexible Proxy Layer}\\
in IEEE Xplore as part of the proceeding of: \\
International Confernce on Future Generation Communication 2012, At London, UK
}\\

\ecvTagPlainValueRagged{}
{Heupel, M.; Fischer, L.;  Bourimi, M.; Kesdoǧan, D. Scerri, S.; Hermann, F.; Gimenez, R.\\
\emph{Context-aware, trust-based access control for the digital.me userware}\\
proceedings o 2012 5th IFIP International Conference on New Technologies, Mobility and Security (NTMS)}\\

-}
genericRighthandEntry:: R.RDF a => a -> R.Subject -> Latex
genericRighthandEntry g s = '{': (intercalate "\\\\\n" [authors,
                                                        title,
                                                        publicationdata]
                                 )
                            ++ "}\n"
                   where
                     authors = latexCommasAnd $ map (R.view) $ authorsOf g s
                     title =   case titlesOf g s of
                       [] -> ""
                       titles -> R.view $ head titles
                     publicationdata = case sequence
                                            $ filter (/= Nothing)
                                              [booktitle, publisher, pages, year] of
                                         Nothing -> ""
                                         Just a -> (intercalate ", " a) 
                     booktitle = booktitleOf g s >>= return.(R.view)-- FIXME no title
                     publisher = Nothing -- ToDo
                     pages = Nothing --ToDo
                     year:: Maybe Latex
                     year = maybe Nothing (Just.(R.view)) (yearOf g s)
                     

genericLefthandEntry:: String -> Latex
genericLefthandEntry s = "\\ecvTagPlainValueRagged{" ++ s ++ "}"
{-| Given an ordered list of publications (RDF subjects), generate entry list.
-}
publicationYear:: R.RDF a => a -> [R.Subject] -> Latex
publicationYear _ [] = mempty
publicationYear g xs@(s:ss) = foldr (\(l,r) x -> l ++ r ++ x) mempty
                              $ zip
                              (yearEntry:(repeat $ genericLefthandEntry ""))
                              (map (genericRighthandEntry g) xs)
  where
    yearEntry = genericLefthandEntry $ maybe "" (R.view) (yearOf g s)




{-| Only the first entry of every year displays the year in the left column. This is the function to call
TODO: sortByYear
-}
genericPublicationList:: R.RDF a => a ->  [R.Subject] -> Latex
genericPublicationList g xs = foldr (\ys l -> publicationYear g ys ++ l) mempty
                              $ groupByYear g
                              $ reverse
                              $ sortBy (compareSubjectYear g) xs


{-| Example List of Publications.
TODO: fill up with my publications.
TODO: read from generic config/RDF (better use RDF-graph to determine them)
-}
myPublications :: R.RDF a => a -> R.Subject -> [R.Subject]
myPublications g s = map ((checknode g).publicationNode)
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



{-| The following functions provide LaTeχ output of teaching experience based on my very crdue first try on using RDF modelling for this.

You find a set of predicates that indicate which contents may be included.


TODO: multi-language for academic terms
-}

{-| these are actually modelled in turtle -}
--teachingPrefixMappings = N.mergePrefixMappings N.standard_ns_mappings $ N.ns_mappings [ N.mkPrefixedNS' "teach" "http://linkedscience.org/teach/ns#"]


practiseTypeObj = R.bnode $ T.pack ":Practise"
seminarTypeObj  = R.bnode $ T.pack ":Seminar"
ctfTypeObj      = R.bnode $ T.pack ":CtF"
courseTypeObj   = mkUnode' teach "Course"
lectureTypeObj  = mkUnode' teach "Lecture"

latexAppendMaybe::  Maybe [a] -> [a] -> [a]
latexAppendMaybe a b = fromMaybe mempty $ a >>= return.(++ b)

teachRightHandEntry:: R.RDF a => a -> R.Subject ->  Latex
teachRightHandEntry g s = fromMaybe "" ((typeOf g s >>= eventTypeStr) >>= return.(++ ": "))
                          ++ (maybe "" (R.view) (titleOf g s)) 
                          where
                            academicTermStr g s = maybe "" (R.view) 
                                                  $ (academicTermOf g s) >>= (nameOf g) 
                            eventTypeStr e | e == practiseTypeObj = Just "Practise"
                                           | e == seminarTypeObj  = Just "Seminar"
                                           | e == lectureTypeObj  = Just "Lecture"
                                           | e == ctfTypeObj      = Just "Capture-the-Flag"
                                           | e == courseTypeObj   = Just "Course"
                                           | otherwise            = Nothing
                              



teachLatexYear:: R.RDF a => a ->  [R.Subject] -> Latex
teachLatexYear g xs =  observe "teachLatexYear"
                       $ foldr (\(l,r) x -> l ++ r ++ x) mempty
                       $ zip
                         ((genericLefthandEntry $ maybe "" (shortNameOf g) (academicTermOf g $ head xs))
                          :(repeat $ genericLefthandEntry ""))
                         $ map (\x -> "{" ++ (teachRightHandEntry g x) ++ "}\n") xs
  where
    shortNameOf:: R.RDF a => a -> R.Subject -> String
    shortNameOf g s = maybe "" (R.view) (nameOf g s) -- TODO use abbrev



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
                              


genericTeachingList:: R.RDF a => a -> [R.Subject] -> Latex
genericTeachingList g = intercalate("\n")
                        .(map $ (teachLatexYear g))
                        .(groupByAcademicTerm g)
                        .(sortTeachingEntries g)  




{- Generate LaTeX Contact Infos -}
contactInfo:: R.RDF a => a -> Latex
contactInfo g = "\\ecvTagPlainValueRagged{}{\\ecvBold{" ++ foafNameView g ++" }\\\\\n"
                ++ replaceString (foafTitleView g) "." ".\\,"
                ++ "Dipl.-Inf.}\n"
                ++ "\\ecvNewLine\n"
                ++ "\\ecvTagPlainValueRagged{\\ecvContact}\n"
                ++ "{" ++ vcardStreetAddr mvCardNode ++ "\\\\\n" 
                ++ vcPostalCode mvCardNode ++ " " ++ vcLocality mvCardNode ++ "\\\\[1mm]\n" 
                ++ "\\ecvMobile: " ++ vcCellphone mvCardNode ++ "\\\\\n" -- TODO prettyprint number
                ++ "\\ecvEmail: \\ecvHyperEMail{"++ foafMboxView g ++ "}\\\\\n"
                ++ "\\ecvHyperLink{"++ foafHomepageView g ++"}"
                ++ "}\n"

                 where
                   mvCardNode = listToMaybe $ vcardHasAddressPred g meNode
                   vcardStreetAddr vc = mHeadObjView g "Addr missing" vc (Just vcStreetAddressNode)
                   vcPostalCode vc    = mHeadObjView g "postal code missing" vc (Just vcPostalCodeNode)
                   vcLocality vc      = mHeadObjView g "locality missing" vc (Just vcLocatityNode)
                   vcCellphone  vc   = fromMaybe "Cellphone missing"
                                       $ (\l -> listToMaybe l >>= (listToMaybe.(vcHasValue g)) >>= return.(R.view))
--                                       $ filter  (isVcCellType g)
                                       $ mvcPhoneNodes
                   vcHasValue g sub = queryObjects g sub vcHasValueNode
                   mvcPhoneNodes  = mQueryObjects g (Just meNode) (Just vcHasTelephoneNode)
                   isVcCellType:: R.RDF a => a -> R.Subject -> Bool
                   isVcCellType g phone = not.null $ R.query g (Just phone) (Just typePred) (Just vcCellTypeNode)
                   
                   vcardHasAddressPred = objectsByPred (mkUnode' vcard "hasAddress")
                   vcStreetAddressNode = mkUnode' vcard "street-address"
                   vcPostalCodeNode = mkUnode' vcard "postal-code"
                   vcLocatityNode = mkUnode' vcard "locality"
                   vcCellTypeNode = mkUnode' vcard "Cell"
                   vcHasTelephoneNode = mkUnode' vcard "hasTelephone"
                   vcHasValueNode = mkUnode' vcard "hasValue"

                   
{-| generate a CV of a given person (english default)
-}
genCvLatex:: R.RDF a => a -> R.Subject -> LatexConfig ->  IO Latex
genCvLatex g s c = (sequence $ cvstructure )
                 >>= return.(foldr ((++)) "")
  where
    latexHeader = readFile $ basedir ++ "header.tex" 
    latexFooter = readFile $ basedir ++ case lang c of
      DE -> "footer.de.tex"
      otherwise -> "footer.tex"
    -- contactInfo = readFile $ basedir ++ "contactInfo.tex"
                  
    experience  = (readFile (basedir ++ "experience.tex"))  -- TODO derive from RDF
                  >>= return.((++) (experienceTitle (lang c))) 
    pubList     = return $ (++) pubListTitle
                  $ genericPublicationList g
                  $ myPublications g s
    teaching    = return $ (++) (teachingTitle (lang c))
                  $ genericTeachingList g
                  $ allCourses g
    education   = readFile $ basedir ++  "education.tex"  -- TODO derive from RDF
    talks       = readFile $ basedir ++ "talks.tex"   -- TODO derive from RDF
    voluntaryWork = (readFile $ basedir ++ "volunteer.tex") -- TODO derive from RDF
                    >>= return.((++) (volunteerTitle (lang c)))
-- ToDo:    abilities, projects 
    cvstructure = [ return $ documentclass c
                  , latexHeader 
                  , return $ contactInfo g
                  , experience 
                  , pubList 
                  , education
                  , talks
                  , voluntaryWork
                  , teaching
                  , latexFooter]



teachingTitle DE = "\\ecvSection{Lehrerfahrung}"
teachingTitle EN = "\\ecvSection{Teaching \\ecvExperience}"
teachingTitle _  = "\\ecvSection{Teaching}"

experienceTitle _ = "\\ecvBreakSection{\\ecvExperience}"
pubListTitle = "\\ecvSection{\\ecvPublications{}}"
volunteerTitle _ = sectionStr "\\ecvVolunteer{}"

pagebreak = "\\ecvPageBreak\n"

sectionStr title = "\\ecvSection{" ++ title ++ "}\n"


documentclass:: LatexConfig -> Latex
documentclass c = concat ["\\documentclass[",
                          intercalate ", " ["utf8", "a4paper", "12pt", "oneside"
                                           ,  case lang c of
                                             DE -> "german"
                                             otherwise -> "english"
                                             ]
                          , "]{ecv-plus}"]





basedir="/home/lars/privat/personalia/templates/"
