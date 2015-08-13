{-# LANGUAGE FlexibleContexts,MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

{-| ToDo: copy to separate Vocabulary handlers (see Data/NS/..)
|-}
module RdfHandler where

import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.RDF as R
import qualified Data.RDF.Namespace as N
import Data.RDF.Types
import Debug.Trace

import LanguageSelector
-- import Debug.Hood.Observe


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



meNode = R.bnode $ T.pack "_:me" -- TODO make nice


mkUnode':: N.Namespace -> String -> R.Node
mkUnode' ns = (R.unode).(N.mkUri ns).(T.pack)

strRnode:: String -> R.Node
strRnode = (R.unode).(T.pack) 



literalTriple:: R.Subject -> R.Predicate -> T.Text -> R.Triple
literalTriple s p t = R.triple s p (R.lnode $ R.plainL t)


type TripleGenFkt = R.Subject -> T.Text -> R.Triple

typePred = mkUnode' rdf "type"


{-| Among a list of LNodes, find the one best matching a given language.
Sort Nodes by similarity to selected language. |-}
languageSelect:: Language -> [R.Node] -> R.Node
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



-- ToDo create instances R.View R.Node String
lvalue (R.LNode (R.PlainL v)) = v


-- FIXME Non-exhaustive patterns in function view
instance R.View R.Node String where
  view (R.UNode t) = T.unpack t
  view (R.BNode t) = T.unpack t
  view (R.BNodeGen i) = show i 
  view (R.LNode (R.PlainL t)) = T.unpack t
  view (R.LNode (R.PlainLL t l)) = T.unpack t -- FIXME, how to handle language
  view (R.LNode (R.TypedL t u)) = T.unpack t -- FIXME how to handle encoding url
  
instance R.View R.Node T.Text where
  view (R.UNode t) = t
  view (R.BNode t) = t
  view (R.LNode (R.PlainL t)) = t
  view _ = undefined




--- Searching and finding
-----------------------

{-| Query for Objects given Subject and Predicate.
-}
queryObjects:: R.RDF a => a -> R.Subject -> R.Predicate -> [R.Object]
queryObjects g s p = map R.objectOf
                     -- $ observe ("queryObj " ++ (R.view s) ++ " " ++ (R.view p))
                     $ R.query g (Just s) (Just p) Nothing

{-| Query for Objects given Subject and Predicate.
-}
mQueryObjects:: R.RDF a => a -> Maybe R.Subject -> Maybe R.Predicate -> [R.Object]
mQueryObjects g s p = map R.objectOf
                     $ R.query g s p Nothing

{-| Convenience function to access queryObjects with reversed parameters. 
-}
objectsByPred:: R.RDF a => R.Predicate -> a -> R.Subject -> [R.Object]
objectsByPred = flip (flip . queryObjects)

headOrFailNode:: String -> [R.Node] -> R.Node
headOrFailNode s ns = case ns of
  [] -> R.bnode $ T.pack s
  xs -> head xs


querySubjects:: R.RDF a => a -> R.Predicate -> R.Object -> [R.Subject]
querySubjects g p o = map R.subjectOf
                      $ R.query g Nothing (Just p) (Just o)



{-| Get the first result of a list of objects matching subject and predicate as string, or an error string
-}
headObjView:: R.RDF a => a -> String -> R.Subject -> R.Predicate -> String
headObjView g err subj pred = fromMaybe err (listToMaybe $ queryObjects g subj pred
                                             >>= return.(R.view))
                              
{-| On a second thought: Documentation would have been good. |-}
headObjViewMod g err fmod subj pred = fromMaybe err (listToMaybe $ queryObjects g subj pred
                                             >>= return.fmod.(R.view))

mHeadObjView:: R.RDF a => a -> String -> Maybe R.Subject -> Maybe R.Predicate -> String
mHeadObjView g err subj pred = fromMaybe err (listToMaybe $ map R.objectOf (R.query g subj pred Nothing)
                                              >>= return.(R.view))
