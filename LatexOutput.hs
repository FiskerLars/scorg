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
import Data.RDF.Types
import Debug.Trace

import Data.DateTime

-- import Debug.Hood.Observe


import Data.RDF.Bibliography
import RdfHandler
import AcademicTerm

import LatexConfig

import Data.RDF.NS.Foaf
import Data.RDF.NS.VCard

import LanguageSelector
import Data.RDF.CV

import Data.RDF.NS.Foaf
import Data.RDF.NS.VCard
import Data.RDF.NS.Teach
import Data.RDF.NS.Bio

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
ecvPublicationRight:: R.RDF a => a -> R.Subject -> Latex
ecvPublicationRight g s = '{': (intercalate "\\\\\n" [authors,
                                                        title,
                                                        publicationdata]
                                 )
                            ++ "}\n"
                   where
                     authors = "\\ecvBold{"++ (latexCommasAnd $ map (R.view) $ authorsOf g s) ++ " }"
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
                     

ecvGenericLeftSide:: String -> Latex
ecvGenericLeftSide s = "\\ecvTagPlainValueRagged{" ++ s ++ "}"
{-| Given an ordered list of publications (RDF subjects), generate entry list.
-}
publicationYear:: R.RDF a => a -> [R.Subject] -> Latex
publicationYear _ [] = mempty
publicationYear g xs@(s:ss) = foldr (\(l,r) x -> l ++ r ++ x) mempty
                              $ zip
                              (yearEntry:(repeat $ ecvGenericLeftSide ""))
                              (map (ecvPublicationRight g) xs)
  where
    yearEntry = ecvGenericLeftSide $ maybe "" (R.view) (yearOf g s)




{-| Only the first entry of every year displays the year in the left column. This is the function to call
TODO: sortByYear
-}
genericPublicationList:: R.RDF a => a ->  [R.Subject] -> Latex
genericPublicationList g xs = foldr (\ys l -> publicationYear g ys ++ l) mempty
                              $ groupByYear g
                              $ reverse
                              $ sortBy (compareSubjectYear g) xs
{-=======


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
>>>>>>> 52e0241b7e40ddd318d7f01b6d8081768be36cf5
-}


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
                              


-----------------------------------------------------
-- Teaching Lists



teachLatexYear:: R.RDF a => a ->  [R.Subject] -> Latex
teachLatexYear g xs =  -- observe "teachLatexYear" $
                        foldr (\(l,r) x -> l ++ r ++ x) mempty
                       $ zip
                         ((ecvGenericLeftSide $ maybe "" (shortNameOf g) (academicTermOf g $ head xs))
                          :(repeat $ ecvGenericLeftSide ""))
                         $ map (\x -> "{" ++ (teachRightHandEntry g x) ++ "}\n") xs
  where
    shortNameOf:: R.RDF a => a -> R.Subject -> String
    shortNameOf g s = maybe "" (R.view) (nameOf g s) -- TODO use abbrev



{- removed by Merge 
=======

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
                              

>>>>>>> 52e0241b7e40ddd318d7f01b6d8081768be36cf5
-}


genericTeachingList:: R.RDF a => a -> [R.Subject] -> Latex
genericTeachingList g = intercalate("\n")
                        .(map $ (teachLatexYear g))
                        .(groupByAcademicTerm g)
                        .(sortTeachingEntries g)  








{- Generate LaTeX Contact Infos -}
ecvContactData:: R.RDF a => LatexConfig -> a -> Latex
ecvContactData c g = "\\ecvTagPlainValueRagged{}{\\ecvBold{" ++ foafNameView g ++" }\\\\\n"
                     ++ replaceString (foafTitleView g) "." ".\\,"
                     ++ "Dipl.-Inf.}\n"
                     ++ "\\ecvNewLine\n"
                     ++ "\\ecvTagPlainValueRagged{\\ecvContact}\n"
                     ++ "{" ++ vcardStreetAddrView g mvAddrCard ++ "\\\\\n" 
                     ++ vcPostalCodeView g mvAddrCard ++ " " ++ vcLocalityView g mvAddrCard ++ "\\\\[1mm]\n" 
                     ++ "\\ecvMobile: " ++ replaceString (vcCellphoneView g (filterVcCells g mvPhoneCards)) "tel" "" ++ "\\\\\n" -- TODO prettyprint number
                     ++ "\\ecvEmail: \\ecvHyperEMail{"++ foafMboxView g ++ "}\\\\\n"
                     ++ "\\ecvHyperLink{"++ foafHomepageView g ++"}"
                     ++ "}\n"
                     
  where
    mvAddrCard = listToMaybe $ vcardHasAddressPred g meNode
    mvPhoneCards = mvcPhoneNodes g meNode


ecvTeachingTitle:: LatexConfig -> Latex
ecvTeachingTitle c | lang c == read "de" = ecvSection "Lehrerfahrung"
                   | lang c == read "en" = ecvSection "Teaching \\ecvExperience"
                   | otherwise           = ecvSection "Teaching"


ecvExperienceTitle _ = ecvSection "\\ecvExperience{}"
ecvPubListTitle _    = ecvSection "\\ecvPublications{}"
ecvVolunteerTitle _  = ecvSection "\\ecvVolunteer{}"

ecvSection title = "\\ecvSection{" ++ title ++ "}\n"

ecvNewPage:: Latex
ecvNewPage = "\\ecvPageBreak\n"


ecvSince:: LatexConfig -> Latex
ecvSince conf | lang conf == read "de" = "seit"
              | otherwise              = "since"


ecvUntil::LatexConfig -> Latex
ecvUntil conf | lang conf == read "de" = "bis"
              | otherwise              = "until"



ecvDate:: LatexConfig -> Maybe DateTime -> Latex
ecvDate _ Nothing = ""
ecvDate _ (Just dt) = formatDateTime "\\ecvDate{%d}{%m}{%y}" dt

{-| \\ecvDate{}{04}{2011} -- \\ecvDate{}{03}{2013} |-}

ecvEmploymentInterval:: LatexConfig -> BioInterval -> Latex
ecvEmploymentInterval conf (BioInterval from to) = case map (ecvDate conf) [from,to] of
                                                     ["", (a:as)]   -> intercalate " " [ecvUntil conf, a:as]
                                                     [ (b:bs), ""]  -> intercalate " " [ecvSince conf, b:bs]
                                                     [ "", ""] -> ""
                                                     [ a, b]         -> intercalate " - " [a, b]
                                                     



{-| Display an employment entry.
|-}
ecvEmploymentEntry:: R.RDF a => LatexConfig -> a -> R.Subject  -> Latex
ecvEmploymentEntry conf g e  = "\\ecvTagPlainValueRagged{"
                              ++ (fromMaybe
                                  "{\\bf no interval given}"
                                  (getBioInterval g e
                                   >>= (return.(ecvEmploymentInterval conf))))
                              ++ "}{"
                              -- ToDo: replace all newlines by \\
                              --       format string
                              ++ ecvMultilineFirstBold conf g e (mkUnode' rdf "label")
                              ++ ecvMultilineString conf g e (mkUnode' rdf "place")
                              ++ "}\n"


ecvMultilineFirstBold:: R.RDF a => LatexConfig -> a -> R.Subject  -> R.Predicate -> Latex
ecvMultilineFirstBold conf g e p = case filter isLNode $ queryObjects g e p of
  [] -> ""
  list -> intercalate "\\\\\n"
          $ (\(l:ls) -> (:) ("\\ecvBold{" ++ l ++  "}") ls) 
          $ lines
          $ R.view
          $ languageSelect (lang conf) list


ecvMultilineString:: R.RDF a => LatexConfig -> a -> R.Subject  -> R.Predicate -> Latex
ecvMultilineString conf g e p = case filter isLNode $ queryObjects g e p of
  [] -> ""
  list -> intercalate "\\\\\n" $ lines
          $ R.view
          $ languageSelect (lang conf) list



    


ecvFormatEmployments:: R.RDF a => LatexConfig -> a ->  [R.Subject]  -> Latex
ecvFormatEmployments conf g = (intercalate "\n")
                              .(map (ecvEmploymentEntry conf g))

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
    -- ecvContactData = readFile $ basedir ++ "contactInfo.tex"
                  
    experienceFile  = (readFile (basedir ++ "experience.tex"))  -- TODO derive from RDF
                  >>= return.((++) (ecvExperienceTitle c)) 
    experience  = return $ (++) (ecvExperienceTitle c)
                  $ ecvFormatEmployments c g
                  $ (\e -> trace ("sorted: " ++ show e) e)
                  $ sortEmployments g
                  $ allEmployments g meNode 
    pubList     = return $ (++) (ecvPubListTitle c)
                  $ genericPublicationList g
                  $ myPublications g s
    teaching    = return $ (++) (ecvTeachingTitle c)
                  $ genericTeachingList g
                  $ allCourses g
    education   = readFile $ basedir ++  "education.tex"  -- TODO derive from RDF
    talks       = readFile $ basedir ++ "talks.tex"   -- TODO derive from RDF
    voluntaryWork = (readFile $ basedir ++ "volunteer.tex") -- TODO derive from RDF
                    >>= return.((++) (ecvVolunteerTitle c))
-- ToDo:    abilities, projects 
    cvstructure = [ return $ documentclass c
                  , latexHeader 
                  , return $ ecvContactData c g
--                  , experienceFile
                  , experience
                  , (return ecvNewPage)
                  , pubList 
                  , education
                  , talks
                  , voluntaryWork
                  , teaching
                  , latexFooter]



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




{-
=======




{- Generate LaTeX Contact Infos -}
contactInfo:: R.RDF a => a -> Latex
contactInfo g = "\\ecvTagPlainValueRagged{}{\\ecvBold{" ++ foafNameView g ++" }\\\\\n"
                ++ replaceString (foafTitleView g) "." ".\\,"
                ++ "Dipl.-Inf.}\n"
                ++ "\\ecvNewLine\n"
                ++ "\\ecvTagPlainValueRagged{\\ecvContact}\n"
                ++ "{" ++ vcardStreetAddrView g mvCardNode ++ "\\\\\n" 
                ++ vcPostalCodeView g mvCardNode ++ " " ++ vcLocalityView g mvCardNode ++ "\\\\[1mm]\n" 
                ++ "\\ecvMobile: " ++ vcCellphoneView g mvCardNode ++ "\\\\\n" -- TODO prettyprint number
                ++ "\\ecvEmail: \\ecvHyperEMail{"++ foafMboxView g ++ "}\\\\\n"
                ++ "\\ecvHyperLink{"++ foafHomepageView g ++"}"
                ++ "}\n"

                 where
                   mvCardNode = listToMaybe $ vcardHasAddressPred g meNode

                   
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
>>>>>>> 52e0241b7e40ddd318d7f01b6d8081768be36cf5
-}

documentclass:: LatexConfig -> Latex
documentclass c = concat ["\\documentclass[",
                          intercalate ", " ["utf8", "a4paper", "12pt", "oneside"
                                           ,  case lang c of
                                             DE -> "german"
                                             otherwise -> "english"
                                             ]
                          , "]{ecv-plus}"]





basedir="/home/lars/privat/personalia/templates/"
