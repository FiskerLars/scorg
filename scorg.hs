{-# LANGUAGE ScopedTypeVariables,
OverloadedStrings, RankNTypes, ImpredicativeTypes #-}
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

import Data.List
import Data.RDF as R
import qualified Data.RDF.Namespace as N
import Data.RDF.Query as Q
import qualified Data.Text as T
import System.Environment
import System.Console.GetOpt

import Debug.Trace
-- import Debug.Hood.Observe

-- import TextUi
import BibTeXInput
import LatexOutput
import LatexConfig
import LanguageSelector
import Options

import CLIarguments

import Data.RDF.CV
import Data.RDF.GraphDB
-- import Database.HSparql.QueryGenerator



    
persons:: RDF r => r -> [R.Subject]
persons g = map Q.subjectOf
            $ query g (Nothing)
                      (Just $ unode "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                      (Just $ unode "http://xmlns.com/foaf/spec/Person")  -- FIXME foldl into difflist (remove Nothings, eventually return empty list)


{- List all Persons (by name) and related email addresses -}
emailStringList:: RDF r => r -> [(String,[String])]
emailStringList gr = map (\s -> (show $ objectOf $ head $ query gr (Just s) (Just $ unode "http://xmlns.com/foaf/spec/name") Nothing,
                                 map (show.objectOf) $ query gr (Just s) (Just $ unode "http://xmlns.com/foaf/spec/mbox") Nothing))
                     $ persons gr






-------------------------------------------------------------------
--- CmdArguments



readGraphs:: Options -> IO Graph
readGraphs opts = do
  bg <- bibtexGraphs    $ inputBibTex opts
  lg <- parseMergeLocal $ inputGraphs opts
  return (mergeGraphs [bg, lg]) 

bibtexGraphs bf = (sequence $ map (parseBibTeX) bf)
                  >>= (\x -> (return.bibTeXToRDF.concat) x:: IO R.TriplesGraph)


main:: IO ()
main = --runO $
       do
         (action, opts, nonOpts) <- parseArgs
         graph <- readGraphs opts
         action graph opts

         
