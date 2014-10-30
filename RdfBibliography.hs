{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}

{-| Local handlers to import and export bibliogräphic RDF.
essentially defines our local terminology.

-}

module RdfBibliography where



import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.RDF as R
import qualified Data.RDF.Namespace as N

import Debug.Hood.Observe

instance Observable R.Node where
  observer = observeBase

instance Observable R.Triple where
  observer = observeBase

pubGraph:: R.RDF a => R.Triples -> a
pubGraph triples = R.mkRdf triples Nothing biboMapping 
  where
    addPubPrefix g = R.addPrefixMappings g biboMapping True
    biboMapping = N.mergePrefixMappings N.standard_ns_mappings $ N.ns_mappings [ bibo ]

bibo:: N.Namespace
bibo  = N.mkPrefixedNS' "bibo" "http://purl.org/ontology/bibo/#"
teach = N.mkPrefixedNS' "teach" "http://linkedscience.org/teach/ns#"
event = N.mkPrefixedNS' "event" "http://purl.org/NET/c4dm/event.owl#"
rdf = N.rdf
dct = N.dct



mkUnode':: N.Namespace -> String -> R.Node
mkUnode' ns = (R.unode).(N.mkUri ns).(T.pack)

strRnode:: String -> R.Node
strRnode = (R.unode).(T.pack) 

publicationNode id = (R.bnode $ T.pack $ ":" ++ id)


literalTriple:: R.Subject -> R.Predicate -> T.Text -> R.Triple
literalTriple s p t = R.triple s p (R.lnode $ R.plainL t)


type TripleGenFkt = R.Subject -> T.Text -> R.Triple

-- ToDo create a new foaf:Person if not existing in the graph
typePred = mkUnode' rdf "type"

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
queryObjects:: R.RDF a => a -> R.Subject -> R.Predicate -> [R.Object]
queryObjects g s p = map R.objectOf
                     $ observe ("queryObj " ++ (R.view s) ++ " " ++ (R.view p))
                     $ R.query g (Just s) (Just p) Nothing
objectsByPred:: R.RDF a => R.Predicate -> a -> R.Subject -> [R.Object]
objectsByPred = flip (flip . queryObjects)

listhead = Data.List.head

typesOf = objectsByPred typePred
typeOf  = (listhead.).typesOf -- listhead.(typesOf g) 
authorsOf = objectsByPred authorPred
editorsOf = objectsByPred editorPred
titlesOf  = objectsByPred titlePred
titleOf   = (listhead.).titlesOf
namesOf   = objectsByPred namePred
nameOf    = (listhead.).namesOf
abbreviationsOf = objectsByPred abbreviationPred
yearsOf = objectsByPred yearPred
yearOf  = (listhead.).yearsOf
booktitleOf = objectsByPred booktitlePred
booktitleof = booktitleOf --Legacy
pdfurlOf    = objectsByPred pdfurlPred -- flip (queryObjects g) pdfurlPred
timestampsOf  = objectsByPred timestampPred 
timestampOf   = (listhead.).timestampsOf

-- teaching experiences
academicTermsOf:: R.RDF a => a -> R.Subject -> [R.Object]
academicTermsOf = objectsByPred academicTermPred  
academicTermOf  = (listhead.).academicTermsOf 


-- ToDo create instances R.View R.Node String
lvalue (R.LNode (R.PlainL v)) = v


instance R.View R.Node String where
  view (R.UNode t) = T.unpack t
  view (R.BNode t) = T.unpack t
  view (R.LNode (R.PlainL t)) = T.unpack t
  view (R.LNode (R.PlainLL t l)) = T.unpack t -- FIXME, how to handle language
  view (R.LNode (R.TypedL t u)) = T.unpack t -- FIXME how to handle encoding url
  
instance R.View R.Node T.Text where
  view (R.UNode t) = t
  view (R.BNode t) = t
  view (R.LNode (R.PlainL t)) = t
  view _ = undefined
