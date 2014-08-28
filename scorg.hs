{-# LANGUAGE ScopedTypeVariables,
OverloadedStrings #-}
{-| ScOrg Science Process Organisation Tool

* Contacts
* Conferences
* Papers

-}
{- Anforderungen
(10:01:45) jens: Scholar ist gut bei Papern
(10:02:08) jens: Research gate vielleicht was Kontakte angeht. Finde ich
aber eigentlich deutlich zu oberflächlich
(10:03:16) jens: ORCID und researcherid gehen zwar in die Richtung
eigene Arbeiten eindeutig identifizierbar zu sammeln, aber die finde ich
sehr unübersichtlich und viel zu wenig automatisch
(10:05:56) jens: bisher favorisiere ich scholar mit etwas handarbeit
-}


import Data.RDF as R
import qualified Data.Text as T

import TextUi

parseLocal:: String -> IO R.TriplesGraph
parseLocal file = R.parseFile (R.TurtleParser Nothing Nothing) file >>= (\r ->
                                              case r of
                                                (Left _) -> return R.empty
                                                (Right rdf) -> return rdf )
                                                               

subject:: R.Triple -> R.Subject
subject (Triple s _ _) = s

predicate:: R.Triple -> R.Predicate
predicate (Triple _ p _) = p

object:: R.Triple -> R.Object
object (Triple _ _ o) = o


persons:: RDF r => r -> [R.Subject]
persons g = map subject $ query g (Nothing) (Just $ unode "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")(Just $ unode "http://xmlns.com/foaf/spec/Person")  -- FIXME foldl into difflist (remove Nothings, eventually return empty list)


{- List all Persons (by name) and related email addresses -}
emailStringList:: RDF r => r -> [(String,[String])]
emailStringList gr = map (\s -> (show $ object $ head $ query gr (Just s) (Just $ unode "http://xmlns.com/foaf/spec/name") Nothing,
                                 map (show.object) $ query gr (Just s) (Just $ unode "http://xmlns.com/foaf/spec/mbox") Nothing))
                     $ persons gr


  
main = parseLocal "/home/lars/etc/contacts.turtle" >>= (\g -> runUi g)-- return.emailStringList >>= print
