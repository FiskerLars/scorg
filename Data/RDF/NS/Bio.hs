{-| A module to handle the bio vocabulary http://vocab.org/bio/0.1.
|-}
module Data.RDF.NS.Bio where

import Data.List
import Data.Maybe
import Data.Time.Format
import Data.DateTime 
import qualified Data.Text as T
import qualified Data.RDF as R
import qualified Data.RDF.Namespace as N

import Debug.Trace

import RdfHandler

bio = N.mkPrefixedNS' "bio" "http://purl.org/vocab/bio/0.1/"

{-| Returns all employments of a given subject. First queries for all subjects of type Employment, then filters for all that have "me" as Principal object.
Todo: ignore case of predicates
|-}
allEmployments:: R.RDF a => a -> R.Subject -> [R.Subject]
allEmployments g s = filter (\e -> (not.null)
                                   $ R.query g (Just e) (Just $ mkUnode' bio "Principal") (Just s))
                     $ querySubjects g typePred (mkUnode' bio "Employment") 

{-| Retrieve the interval of a given event. |-}
intervalOf:: R.RDF a => a -> R.Subject -> Maybe R.Subject
intervalOf g s = listToMaybe $ queryObjects g s $ mkUnode' bio "eventInterval"


type StartDate = Maybe DateTime
type EndDate   = Maybe DateTime


fromBioDate:: R.RDF a => a -> R.Subject -> Maybe DateTime
fromBioDate g s = headObjViewMod g onError
                  makeDateTime
                  s (mkUnode' bio "date")
  where
    onError = trace ("Error in from BioDate for subject "++(show s)) Nothing
    -- "yyyy-mm-dd"
    makeDateTime = parseDateTime (iso8601DateFormat Nothing)


maybeGetBioDate:: R.RDF a => a -> R.Subject -> R.Predicate -> Maybe DateTime
maybeGetBioDate g s p = (listToMaybe $ queryObjects g s p)
                        >>= fromBioDate g



data BioInterval = BioInterval StartDate EndDate deriving (Eq, Show)

{-| LT if a starts before b.
Something that has no time obviously started earlier (because it runs always).|-}
instance Ord BioInterval where
  compare (BioInterval (Just a) _) (BioInterval (Just b) _) =  compare a b
  

fromIntervalSubject:: R.RDF a => a -> R.Subject -> BioInterval
fromIntervalSubject g s = let (a:b:_) = map (\str ->  maybeGetBioDate g s (mkUnode' bio (trace str str)))
                                        [ "initiatingEvent"
                                        , "concludingEvent"]
                          in BioInterval a b

                             
getBioInterval:: R.RDF a => a -> R.Subject -> Maybe BioInterval
getBioInterval g s = intervalOf g s >>= (return
                                         .(\x -> trace ("BioInt: "++show s++": "++show x) x)
                                         .(fromIntervalSubject g)
                                         .(\x -> trace (show x) x)
                                        )





sortEmployments:: R.RDF a => a -> [R.Subject] -> [R.Subject]
sortEmployments g = reverse.(sortBy employmentOrder)
  where
    employmentOrder a b = let ia = getBioInterval g a
                              ib = trace
                                   ("snd interval: "++ show b++" -> "++ (show $ getBioInterval g b))
                                   getBioInterval g b
                          in case sequence [ia, ib] of
                            Nothing -> error $ "Employment in sorting without Interval ("++show a++" or "++show b++")"
                            (Just [a,b]) -> compare a b


