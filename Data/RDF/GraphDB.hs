{-| Functions to read, write and manipulate graphs
|-}

module Data.RDF.GraphDB  where

import Data.RDF as R
import qualified Data.RDF.Namespace as N

-- import Database.HSparql.QueryGenerator

{-instance Observable R.TriplesGraph where
  observer = observeBase
-}

-- ToDo: this type should be defined in a more scorg-specific module
type Graph = R.TriplesGraph

{-| Parser for turtle file int TriplesGraph
-}
parseLocal:: String -> IO Graph
parseLocal file = (R.parseFile (R.TurtleParser Nothing Nothing) file)
                  >>= (\r -> 
                        case r of
                          (Left err ) -> error $ "Could not parse " ++ (file ++ (show err))-- return R.empty
                          (Right rdf) -> return rdf )
                                                               


parseMergeLocal:: [String] -> IO Graph
parseMergeLocal [] = return R.empty
parseMergeLocal gs = (sequence $  map (parseLocal) gs)
                     >>= return.mergeGraphs


mergeGraphs:: [Graph] -> Graph 
mergeGraphs gs =  (\(m,t) -> R.mkRdf t Nothing m)
                  $ foldl (\(maps,triples) g -> (N.mergePrefixMappings maps (R.prefixMappings g)
                                                 ,R.triplesOf g ++ triples )) (N.ns_mappings [],[]) gs 


