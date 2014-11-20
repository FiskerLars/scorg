
module LatexOutput (Latex,
                    genericPublicationList
                   , genericTeachingList
                   , defaultCourses
                   , genCvLatex)
       where

import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.RDF as R


import Debug.Hood.Observe


import RdfBibliography
import AcademicTerm







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
genericRighthandEntry g s = intercalate "\\\\\n" [authors,
                                                  title,
                                                  publicationdata]
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

groupByYear:: R.RDF a => a -> [R.Subject] -> [[R.Subject]]
groupByYear g  = groupBy (\s s' ->  (yearOf g s) == (yearOf g s'))
                   -- map (\s -> (s, yearOf g s)) xs


{-| Only the first entry of every year displays the year in the left column. This is the function to call
TODO: sortByYear
-}
genericPublicationList:: R.RDF a => a ->  [R.Subject] -> Latex
genericPublicationList g xs = foldr (\ys l -> publicationYear g ys ++ l) mempty $ groupByYear g xs


{-| Example List of Publications.
TODO: fill up with my publications.
TODO: read from generic config/RDF (better use RDF-graph to determine them)
-}
myPublications::[R.Subject]
myPublications = map publicationNode ["fischer06:_secur_revoc_anony_authen_inter",
                                      ""]




{-| The following functions provide LaTeχ output of teaching experience based on my very crdue first try on using RDF modelling for this.

You find a set of predicates that indicate which contents may be included.

\ecvSection{Teaching \ecvExperience}

\ecvBreakSubSection{Thesis Tutor}
\ecvTagPlainValueRagged{SS 2012}{Rania Said: Online Profile Manager\\ (Bachelor Thesis, German University in Cairo)}
\ecvTagPlainValueRagged{WS 2011}{Nils Hahn: Sensor Fusion for Indoor Localisation}


\ecvSubSection{Teaching Assistant}
\ecvTagPlainValueRagged{WS 2011}{Lab Course: Einf\"{u}hrung in die IT-Sicherheit\\
  Seminar: Geometric Routing
}
\ecvTagPlainValueRagged{WS 2005}{Lab Course: Grundlagen der Informatik 3\\[0mm]
  [Fundamentals of Computer Science 3]}


\ecvBreakSubSection{Autonomous Teaching}
\ecvTagPlainValueRagged{WS 2013}{Lecture: Einführung in die IT-Sicherheit\\
Lecture: Security in Mobile Communication\\



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
                         $ map (\x -> "{" ++ (teachRightHandEntry g x) ++ "}") xs
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
                                                 (academicTermOf g s >>= return.readATobj)
                                                 (academicTermOf g s' >>= return.readATobj) -- FIXME: order by termTODO: order Course,Lecture,.. (quality rang)
                            where
                              readATobj:: R.Node -> AcademicTerm
                              readATobj = read.tail.(R.view)
                              


genericTeachingList:: R.RDF a => a -> [R.Subject] -> Latex
genericTeachingList g = intercalate("\n")
                        .(map $ (teachLatexYear g)
                        .(sortTeachingEntries g))
                        .(groupByAcademicTerm g)


defaultCourses = map ((R.unode).(T.pack)) [ "#ws14winfo"
                                          , "#ws14lbas"
                                          , "#ws13post"
                                          , "#ss14mobsec"
                                          , "#ss14kuvs"
                                          , "#ws13post"
                                          , "#ws13itsec"
                                          , "#ws13mobsec"
                                          , "#ws13semsec"
                                          , "#ss13semitsec"
                                          , "#ss13osn"
                                          , "#ss13kuvs"
                                          , "#ss12osn"
                                          , "#ss12pit"
                                          , "#ws11itsec"
                                          , "#ws11locpriv"
                                          , "#ss08secvehic"
                                          , "#ss08da-op3n"
                                          , "#ws07car2car"
                                          , "#ss07hapra"
                                          , "#ss07usfctf"
                                          , "#ws06topoaddr"
                                          , "#ws06ictf"
                                          , "#ss06hapra"
                                          , "#ws05gdi3"
                                          , "#ws05ictf"
                                          , "#ss05its2"
                                          , "#ss05da-op3n"
                                          , "#ws04its1"
                                          , "#ws04ictf"
                                          , "#ss04adhoc"
                                          , "#ws03os" ]




{-| generate a CV of a given person
-}
genCvLatex:: R.RDF a => a -> R.Subject -> Latex
genCvLatex g s = latexHeader
                 ++ contactInfo
                 ++ experience
                 ++ (genericPublicationList g $ observe "myPubs" myPublications)
                 ++ education
                 ++ talks
                 ++ (genericTeachingList g defaultCourses)
                 ++ latexFooter

  where
    latexHeader = ""
    latexFooter = ""
    contactInfo = ""
    experience  = ""
    education   = ""
    talks       = ""