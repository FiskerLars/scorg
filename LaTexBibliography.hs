{-|
This module utilises the ecv-plus latex class to generate a publication list from a given list of ordered BibTeX identifiers.

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

module LaTexBibliography where

import Data.List
import Data.Monoid
import Data.RDF.Bibliography
import qualified Data.Text as T
import qualified Data.RDF as R

type Latex = String

latexCommasAnd:: [Latex] -> Latex
latexCommasAnd [x]   = x
latexCommasAnd [x,y] = x ++ " and " ++ y
latexCommasAnd (x:xs) = x ++ ", " ++ latexCommasAnd xs
latexCommasAnd [] = ""


genericRighthandEntry:: R.RDF a => a -> R.Subject -> Latex
genericRighthandEntry g s = intercalate "\\\\\n" [authors,
                                                  title,
                                                  publicationdata]
                   where
                     authors = latexCommasAnd $ map (R.view) $ authorsOf g s
                     title =  R.view $ head $ titlesOf g s
                     publicationdata = case sequence
                                            $ filter (/= Nothing)
                                              [booktitle, publisher, pages, year] of
                                         Nothing -> ""
                                         Just a -> (intercalate ", " a) 
                     booktitle = Just $ R.view $ head $ booktitleOf g s
                     publisher = Nothing -- ToDo
                     pages = Nothing --ToDo
                     year:: Maybe Latex
                     year = Just $ R.view $ yearOf g s
                     

genericLefthandEntry:: String -> Latex
genericLefthandEntry s = "\\ecvTagPlainValueRagged{" ++ s ++ "}\n"
{-| Given an ordered list of publications (RDF subjects), generate entry list.
-}
publicationYear:: R.RDF a => a -> [R.Subject] -> Latex
publicationYear _ [] = mempty
publicationYear g xs = foldr (\(l,r) x -> l ++ r ++ x) mempty
                       $ zip
                        (genericLefthandEntry (R.view $ yearOf g (head xs)):(repeat $ genericLefthandEntry ""))
                        $ map (genericRighthandEntry g) xs


groupByYear:: R.RDF a => a -> [R.Subject] -> [[R.Subject]]
groupByYear g  = groupBy (\s s' ->  (yearOf g s) == (yearOf g s'))
                   -- map (\s -> (s, yearOf g s)) xs


{-| Only the first entry of every year displays the year in the left column
-}
genericEntryList:: R.RDF a => a ->  [R.Subject] -> Latex
genericEntryList g xs = foldr (\ys l -> publicationYear g ys ++ l) mempty $ groupByYear g xs



myPublications::[R.Subject]
myPublications = map publicationNode ["fischer06:_secur_revoc_anony_authen_inter"]