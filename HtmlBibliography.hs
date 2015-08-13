{-# LANGUAGE Rank2Types, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-| 
Generate Publication Part of Webpage

HTML-Output for bibliography data

-}
import Prelude hiding (span, head)
import Debug.Trace
import Data.List hiding (span)
import qualified Data.Map.Lazy as M
--import System.IO.UTF8

import Publications
import Data.RDF.Bibliography

import qualified Data.Text as T
import qualified Data.RDF as R

import Blaze.Html5
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

-------------HTML Generation ----------------------------

{-| RDFa attributes
see: https://en.wikipedia.org/wiki/Rdfa
-}
rdfaAttribute s = customAttribute (stringTag s)
about = rdfaAttribute "about"
property = rdfaAttribute "property"
typeof = rdfaAttribute "typeof"
content = rdfaAttribute "content"


emptyHtml = toHtml ""
spaceHtml = toHtml " "
nodeHtmlValue n = toValue (R.view n::String)
prefix = customAttribute $ stringTag "prefix" 
httpequiv = customAttribute $ stringTag "http-equiv"

type GenPropertyHtml = R.RDF a => a -> R.Subject -> Html


{-|
TODO: generate links to author Subjects (authors have to be separated beforehand)
-}
authorsHtml:: GenPropertyHtml
authorsHtml g s = htmlCommasAnd
                  $ map (\a -> span
                               ! property (nodeHtmlValue authorPred) 
                               $ toHtml $ (R.view a::String))
                  $ authorsOf g s

pubtitleHtml:: GenPropertyHtml
pubtitleHtml g s = b !. (toValue "pubtitle")
                        ! property (nodeHtmlValue titlePred)
                    $ toHtml $ titlestring
  where titlestring = intercalate " — " $ map ((T.unpack).lvalue) $ titlesOf g s

publisherHtml:: GenPropertyHtml 
publisherHtml g s = toHtml ""

yearHtml:: GenPropertyHtml
yearHtml g s = span
               ! property (toValue (R.view yearPred::String))
               $ toHtml (" (" ++ yearstring ++ ")")
  where
    yearstring =  intercalate " " $ map ((T.unpack).lvalue) $ yearsOf g s

booktitleHtml:: GenPropertyHtml
booktitleHtml g s = case booktitleof g s of
   [] -> emptyHtml
   bs -> toHtml ("in " ++ booktitlestring bs)
  where 
        booktitlestring = (intercalate " ").(map ((T.unpack).lvalue))

editorsHtml:: GenPropertyHtml
editorsHtml g s = span
                  !# (toValue "editors")
                  $ htmlIntercalate spaceHtml
                  [htmlCommasAnd
                   $ map (\a -> span
                                ! property (nodeHtmlValue editorPred)
                                $ toHtml (R.view a::String))
                   $ editorsOf g s
                  , toHtml "(Ed.)"]

{-| Generate links to files.
TODO add RDFa
FIXME strange space before "link"
FIXME debug missing pdfurl
-}
linksHtml:: GenPropertyHtml
linksHtml g s = case linkfields of
  [] -> toHtml ""
  xs -> htmlIntercalate emptyHtml [ toHtml "["
                                  , htmlIntercalate (toHtml "|") (map alink xs)
                                  , toHtml "]"
                                  ]
  where
          linkfields:: R.Triples
          linkfields = R.select g (Just ((==) s)) (Just isLinkPred) Nothing
          isLinkPred:: R.Predicate -> Bool
          isLinkPred p = p == urlPred || p == pdfurlPred

          alink:: R.Triple -> Html
          alink t = if R.predicateOf t == urlPred
                    then a ! href (nodeHtmlValue $ R.objectOf t)
                            $ toHtml "link"
                    else if R.predicateOf t == pdfurlPred
                         then a ! href (nodeHtmlValue $ R.objectOf t)
                              $ toHtml "pdf"
                         else emptyHtml
          

{-| Generate a generic order of Html5+RDFa entry field representations for a given Entry-subject.
-}
genHtmlEntryFields:: R.RDF a => a -> R.Subject -> [Html]
genHtmlEntryFields g s = map (\h -> h g s) entryFields 
  where 
    entryFields:: R.RDF a => [a -> R.Subject -> Html]
    entryFields = [ authorsHtml, (\_ _ -> br)
                  , pubtitleHtml, (\_ _ -> br)
                  , booktitleHtml, publisherHtml
                  , (\_ _ -> spaceHtml), yearHtml
                  , (\_ _ -> spaceHtml), linksHtml ]


{-| Generate a bibliographic entry (list item) from data in graph g for subject s (should be something with authors,...)
TODO: include links to files (if any, or URLS)
-} 
genHTMLEntry:: R.RDF a => a -> R.Subject -> Html 
genHTMLEntry g s = case typesOf g s of
    _ -> li ! about (toValue (R.view s ::String))
         $ do htmlIntercalate (toHtml "") $ genHtmlEntryFields g s



{-| Generate a HTML5 + RDFa unordered list. From list of  BibTeχ identifiers.
TODO: Test
-}
genHTMLListFromIDs:: R.RDF a => a -> [String] -> Html
genHTMLListFromIDs g l = ul $ do linesToHtml $ map ((genHTMLEntry g).(subjectFromID)) l
  where
    subjectFromID:: String -> R.Subject
    subjectFromID id = R.subjectOf $ Data.List.head $ R.query g (Just $ R.unode (T.pack $ ":" ++ id)) Nothing Nothing


{-| Generate a list of all subjects in g that have a title.
TODO: order by year
-}
genHTMLList template g = ul $ do mapM_ (genHTMLEntry g)
                                 $ sortBy (revTimeOrder g)
                                 $ R.listSubjectsWithPredicate g titlePred
{-                         where timeSort::  R.Triples -> R.Triples
                               timeSort = sortBy (\t t' -> compare
                                                            (timeValOf $ R.subjectOf t')
                                                            (timeValOf $ R.subjectOf t)
                                                  )
                               timeValOf:: Ord a => R.Subject -> a
                               timeValOf s = (R.view $ R.objectOf $ Data.List.head
                                              $ R.query g (Just s) (Just yearPred) Nothing) ::T.Text
                                                                                                 
-}

revTimeOrder:: R.RDF a => a -> R.Subject -> R.Subject -> Ordering
revTimeOrder g s s' = case compare (lvalue $ yearOf g s') (lvalue $ yearOf g s) of
  EQ -> compare (lvalue $ timestampOf g s') (lvalue $ timestampOf g s)
  x  -> x
  



{-|
-- headerfields
               {-
               <LINK REL=StyleSheet HREF="style.css" TYPE="text/css" MEDIA=screen>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<link rel="meta" type="application/rdf+xml" title="FOAF"  
               href="http://larsipulami.de/lars_fischer/foaf.rdf"/>
-}
-}
headContents:: R.RDF a => a -> Html
headContents g = htmlIntercalate (emptyHtml)
                 [ link ! rel (toValue "StyleSheet")
                      ! href (toValue "style.css")
                      ! type_ (toValue "text/css")
                      ! media (toValue "screen") 
                 , meta ! httpequiv (toValue "Content-Type")
                        ! Blaze.Html5.content (toValue "text/html; charset=utf-8")
                 , link ! rel (toValue "meta")
                        !type_ (toValue "application/rdf+xml")
                        ! title (toValue "FOAF")
                        ! href (toValue "http://larsipulami.de/lars_fischer/foaf.rdf") -- FIXME
                 ]

{-| Function to append prefix attributes to Html Element.
-}
insertHtmlRdfNamespaces::R.RDF a => a -> Html -> Html
insertHtmlRdfNamespaces = (foldr (\(p,u) -> (! prefix (toValue (p
                                                            `T.append` (T.pack ": ")
                                                            `T.append` u))))
                        toHtml )
                       .(\(R.PrefixMappings m) -> M.toList m)
                       .(R.prefixMappings)

{-
TODO generate prefix attributes for html
TODO add type information (for thesis) to output
-}
genHTML template g = docTypeHtml 
                     $ do (insertHtmlRdfNamespaces g)
                            $ Blaze.Html5.head (headContents g) 
                          body $ do genHTMLList template g


----------------- MAIN -------------------------------

main:: IO ()
main = do
       --hSetEncoding stdout utf8
       parseBibTeX "eigenes.bib"
         >>= return.(\e -> bibTeXToRDF e ::R.MGraph)
         >>= putStr.renderHtml.(genHTML template)
  where template=""
