{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}

{-| Local handlers to import and export bibliogräphic RDF.
essentially defines our local terminology.

-}

module RdfBibliography where



import Data.List
import Data.Maybe
import Control.Applicative

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.RDF as R
import qualified Data.RDF.Namespace as N

import Debug.Hood.Observe

import RdfHandler



bibliographyMappings = N.mergePrefixMappings N.standard_ns_mappings $ N.ns_mappings [ bibo ]

bibliographyMappings = N.mergePrefixMappings N.standard_ns_mappings $ N.ns_mappings [ bibo ]

pubGraph:: R.RDF a => R.Triples -> a
pubGraph triples = R.mkRdf triples Nothing bibliographyMappings
  where
    addPubPrefix g = R.addPrefixMappings g bibliographyMappings True

bibo:: N.Namespace
bibo  = N.mkPrefixedNS' "bibo" "http://purl.org/ontology/bibo/#"

publicationNode id = (R.bnode $ T.pack $ ":" ++ id)




authorPred = mkUnode' dct "creator"
editorPred = mkUnode' bibo "editor"
author:: TripleGenFkt
author = flip literalTriple authorPred 

titlePred = mkUnode' dct "title"
title:: TripleGenFkt
title = flip literalTriple titlePred

namePred = mkUnode' dct "name"
name:: TripleGenFkt
name = flip literalTriple namePred

booktitlePred = mkUnode' dct "partOf"
booktitle:: TripleGenFkt
booktitle = flip literalTriple booktitlePred

publisherPred = mkUnode' dct "publisher"
publisher:: TripleGenFkt
publisher = flip literalTriple publisherPred

-- TODO parse time sensibly
-- FIXME rename year to created
yearPred = mkUnode' dct "created"
year:: TripleGenFkt
year  = flip literalTriple yearPred 

timestampPred = R.unode$ T.pack ":timestamp"
dbDateInserted:: TripleGenFkt
dbDateInserted s t = flip literalTriple timestampPred s t  


filePred = strRnode ":file"

urlPred = strRnode ":url"
url::  TripleGenFkt
url = flip literalTriple urlPred

pdfurlPred = strRnode ":pdfUrl" 
pdfurl:: TripleGenFkt
pdfurl = flip literalTriple pdfurlPred

pagesPred     = mkUnode' bibo "numPages"
startPagePred = mkUnode' bibo "pageStart"
endPagePred   = mkUnode' bibo "pageEnd"
pages::  R.Subject -> T.Text -> R.Triples
pages s t = case T.splitOn (T.pack "-") t of
  (p:[]) -> [literalTriple s pagesPred p]
  (start:end:_) -> [ literalTriple s startPagePred start
                   , literalTriple s endPagePred end ]  



--- teaching Experience

academicTermPred = mkUnode' teach  "academicTerm"
                   --R.unode $ T.pack "http://linkedscience.org/teach/ns#academicTerm"
abbreviationPred = R.unode $ T.pack ":abbreviation"





------------- RDF Handling --------------------------

<<<<<<< HEAD

typesOf = objectsByPred typePred
typeOf  = (listToMaybe.).typesOf 
=======

listhead = Data.List.head

typesOf = objectsByPred typePred
typeOf  = (listToMaybe.).typesOf -- listhead.(typesOf g) 
>>>>>>> a777ac92c3c731b331d6cd061d9dce019ed9e3f6
authorsOf = objectsByPred authorPred
editorsOf = objectsByPred editorPred
titlesOf  = objectsByPred titlePred
titleOf   = (listToMaybe.).titlesOf
namesOf   = objectsByPred namePred
nameOf    = (listToMaybe.).namesOf
abbreviationsOf = objectsByPred abbreviationPred
yearsOf = objectsByPred yearPred
yearOf  = (listToMaybe.).yearsOf
booktitlesOf = objectsByPred booktitlePred
booktitleOf = (listToMaybe.).booktitlesOf 
pdfurlOf    = objectsByPred pdfurlPred -- flip (queryObjects g) pdfurlPred
timestampsOf  = objectsByPred timestampPred 
timestampOf   = (listToMaybe.).timestampsOf

-- teaching experiences
academicTermsOf:: R.RDF a => a -> R.Subject -> [R.Object]
academicTermsOf = objectsByPred academicTermPred  
<<<<<<< HEAD
academicTermOf:: R.RDF a => a -> R.Subject -> Maybe (R.Object)
academicTermOf  = (listToMaybe.).academicTermsOf 


=======
academicTermOf  = (listToMaybe.).academicTermsOf 
>>>>>>> a777ac92c3c731b331d6cd061d9dce019ed9e3f6

{- Group subjects by year-attribute -}
groupByYear:: R.RDF a => a -> [R.Subject] -> [[R.Subject]]
groupByYear g  = groupBy (\s s' ->  (yearInt g s) == (yearInt g s'))
                   -- map (\s -> (s, yearOf g s)) xs


cmpSubjYear::  R.RDF a => a -> R.Subject -> R.Subject -> Maybe Ordering
cmpSubjYear g s s' = pure compare <*> (yearInt g s) <*> (yearInt g s') 

{- Comparision function for Year Attributes -}
compareSubjectYear:: R.RDF a => a -> R.Subject -> R.Subject -> Ordering
compareSubjectYear g s s' = case cmpSubjYear g s s' of
  (Nothing) -> EQ
  (Just x ) -> x
  
yearInt:: R.RDF a => a -> R.Subject -> Maybe Integer
yearInt g s = pure (read.(R.view)) <*> (yearOf g s) 
