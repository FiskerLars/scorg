{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}

{-| ToDo: copy to separate Vocabulary handlers (see Data/NS/..)
|-}
module RdfHandler where

import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T

import Data.RDF
--import Data.RDF.Graph.TList

--import Data.RDF.Types
import qualified Data.RDF.Namespace as N

import Debug.Trace

import LanguageSelector
-- import Debug.Hood.Observe

type RDFdb = RDF TList -- TriplesGraph


{- instance Observable R.Node where
  observer = observeBase

instance Observable R.Triple where
  observer = observeBase
-}



-- General RDF Namespaces 
event = N.mkPrefixedNS' "event" "http://purl.org/NET/c4dm/event.owl#"
vcard = N.mkPrefixedNS' "vcard" "http://www.w3.org/TR/vcard-rdf/"
rdf = N.rdf
dct = N.dct

data Nodes = Nodes {
  name:: Predicate
  }
node = Nodes {
  name = mkUnode' dct "name"
  }

-- meNode = unode $ T.pack "http://larsipulami.de/lars_fischer#me" -- TODO make nice


mkUnode':: N.Namespace -> String -> Node
mkUnode' ns = (unode).(N.mkUri ns).(T.pack)

strRnode:: String -> Node
strRnode = (unode).(T.pack) 



literalTriple:: Subject -> Predicate -> T.Text -> Triple
literalTriple s p t = triple s p (lnode $ plainL t)


type TripleGenFkt = Subject -> T.Text -> Triple

typePred = mkUnode' rdf "type"


{-| Among a list of LNodes, find the one best matching a given language.
Sort Nodes by similarity to selected language. |-}
languageSelect:: Language -> [Node] -> Node
languageSelect lang = head.(sortBy cmpNodeLang)
  where
    cmpNodeLang a b = cmpLang (getValueLang a) (getValueLang b)
    getValueLang (LNode (PlainL _))    = LANG_UNDEF
    getValueLang (LNode (PlainLL _ l)) = read (T.unpack l) ::Language
    getValueLang _             = undefined
    cmpLang LANG_UNDEF b | b == lang = GT
                         | b /= lang = LT
                         | otherwise = EQ
    cmpLang a LANG_UNDEF | a == lang = LT
                         | a /= lang = GT
                         | otherwise = EQ
    cmpLang a b = compare (a /= lang) (b /= lang)



-- ToDo create instances View Node String
lvalue (LNode (PlainL v)) = v


-- FIXME Non-exhaustive patterns in function view
instance View Node String where
  view (UNode t) = T.unpack t
  view (BNode t) = T.unpack t
  view (BNodeGen i) = show i 
  view (LNode (PlainL t)) = T.unpack t
  view (LNode (PlainLL t l)) = T.unpack t -- FIXME, how to handle language
  view (LNode (TypedL t u)) = T.unpack t -- FIXME how to handle encoding url
  
instance View Node T.Text where
  view (UNode t) = t
  view (BNode t) = t
  view (LNode (PlainL t)) = t
  view _ = undefined




--- Searching and finding
-----------------------

{-| Query for Objects given Subject and Predicate.
-}
queryObjects::  RDFdb -> Subject -> Predicate -> [Object]
queryObjects g s p = map objectOf
                     -- $ observe ("queryObj " ++ (view s) ++ " " ++ (view p))
                     $ query g (Just s) (Just p) Nothing

queryObject:: RDFdb -> Subject -> Predicate -> Maybe Object
queryObject g s p = listToMaybe $ queryObjects g s p

{-| Query for Objects given Subject and Predicate.
-}
mQueryObjects:: RDFdb -> Maybe Subject -> Maybe Predicate -> [Object]
mQueryObjects g s p = map objectOf
                     $ query g s p Nothing

{-| Convenience function to access queryObjects with reversed parameters. 
-}
objectsByPred::  Predicate -> RDFdb -> Subject -> [Object]
objectsByPred = flip (flip . queryObjects)

headOrFailNode:: String -> [Node] -> Node
headOrFailNode s ns = case ns of
  [] -> bnode $ T.pack s
  xs -> head xs

headNodeOrError:: String -> [Node] -> Node
headNodeOrError err [] = error err
headNodeOrError _   xs = head xs




querySubjects:: RDFdb -> Predicate -> Object -> [Subject]
querySubjects g p o = map subjectOf
                      $ query g Nothing (Just p) (Just o)



{-| Get the first result of a list of objects matching subject and predicate as string, or an error string
-}
headObjView:: RDFdb -> String -> Subject -> Predicate -> String
headObjView g err subj pred = fromMaybe err (listToMaybe $ queryObjects g subj pred
                                             >>= return.(view))
                              
{-| On a second thought: Documentation would have been good. |-}
headObjViewMod g err fmod subj pred = fromMaybe err (listToMaybe $ queryObjects g subj pred
                                             >>= return.fmod.(view))

mHeadObjView:: RDFdb -> String -> Maybe Subject -> Maybe Predicate -> String
mHeadObjView g err subj pred = fromMaybe err (listToMaybe $ map objectOf (query g subj pred Nothing)
                                              >>= return.(view))

--------Specific Queries ---------------

nameOfNode:: RDFdb -> Subject -> String
nameOfNode g termSubj =  view
  $ fromMaybe (error $ "No dct:name for " ++ (view termSubj))
   (queryObject g termSubj (name node))
  --headObjView g "academicTerm has no name" termSubj (name node)  
