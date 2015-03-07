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

import Debug.Trace
import Debug.Hood.Observe

-- import TextUi
import BibTeXInput
import LatexOutput
import LatexConfig
import LanguageSelector
import RdfCv

import System.Console.GetOpt

instance Observable R.TriplesGraph where
  observer = observeBase


{-| Parser for turtle file int TriplesGraph
-}
parseLocal:: String -> IO R.TriplesGraph
parseLocal file = (R.parseFile (R.TurtleParser Nothing Nothing) file)
                  >>= (\r -> 
                        case r of
                          (Left err ) -> error $ "Could not parse " ++ (file ++ (show err))-- return R.empty
                          (Right rdf) -> return $ observe "RDF parsed" rdf )
                                                               


parseMergeLocal:: [String] -> IO R.TriplesGraph
parseMergeLocal [] = return R.empty
parseMergeLocal gs = (sequence $ observe "parseMergeLocal map" $ map (observe "parseLocal" $ parseLocal) gs)
                     >>= observe "parseMergeLocal" return.mergeGraphs


mergeGraphs gs =  (\(m,t) -> R.mkRdf t Nothing m)
                  $ foldl (\(maps,triples) g -> (N.mergePrefixMappings maps $ R.prefixMappings g
                                                     , observe "merge triples" $ R.triplesOf g ++ triples )) (N.ns_mappings [],[]) gs 

    
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





---------------------------------------------------------------------------------
-- Workers

--parseBibTeX:: String -> IO [Entry.T]
--bibTeXToRDF:: R.RDF a => [Entry.T] -> a
printGraph::Worker
printGraph g _ = putStrLn $ show g


rdfCv:: Worker
rdfCv g _ = putStrLn $ show $ cvParts g (R.bnode $ T.pack "_:me") 

latexcv:: Worker
latexcv g o = ((flip.flip genCvLatex) (R.unode $ T.pack "#me") (genLatexConfig o)) g
              >>= putStrLn 


latexlectures:: Worker
latexlectures g _ = putStrLn $ ((flip genericTeachingList) (allCourses g) ) g
                    
genLatexConfig:: Options -> LatexConfig
genLatexConfig o = LatexConfig {lang = language o}

type Graph = R.TriplesGraph
type Worker = Graph-> Options -> IO () 
data Command = Command String Worker String
commands:: [Command]
commands = [ Command "cvlatex" latexcv
             "Generate a CV of me in LaTeX"
           , Command "lectureslatex" latexlectures
             "Only the lectures part"
           , Command "publicationlatex" undefined
             "Only the Publications (for debugging)"
           , Command "printGraph" printGraph
             "Output the known graph"
           , Command "www" undefined
             "Create My Homepage"
           , Command "rdfcv" rdfCv
             "Test-Output the Cv Rdv (for debugging)"
           , Command "" (\_ _ -> mainUi)
             "Default: run the main ui"
           ]




{- lookup function matching cmd in List of commands -}
lookupCmd:: [Command] -> String -> Maybe Worker
lookupCmd cmds s = let
  cmdName (Command cmd _ _) = cmd
  in case (find (\c -> isPrefixOf s $ cmdName c) cmds) of
    (Just (Command _ w _)) -> Just w
    (Nothing)              -> Nothing


-------------------------------------------------------------------
--- CmdArguments



type ArgStr = [String]


data Options = Options
     { inputGraphs :: [String]
     , inputBibTex :: [String]
     , language:: Language
     , outputType:: String } deriving Show

instance Observable Options where
  observer = observeBase

defaultOptions = Options
     { inputGraphs = []
     , inputBibTex = []
     , language = EN
     , outputType = "rdf"
     }


mainoptions =
  [ Option [] ["ib"]     (ReqArg (\s o -> o {inputBibTex =  inputBibTex o ++ [s] })
                        "bibtex include" )
    "Insert BibTex File"
  , Option [] ["ig"] (ReqArg (\s o -> o { inputGraphs = inputGraphs o ++ [s] })
                      "input include" )
    "Insert a RDF (turtle) File"
  , Option ['l'] ["lang"] (ReqArg (\s o -> o { language = read s })
                           "language")
    "Choose a language, default is en"
  , Option ['o'] ["output"] (ReqArg (\s o -> o { outputType = s }) "output type")
    "Select an output type."
  ]


--- Startups

mainUi:: IO ()                     
mainUi = undefined -- parseLocal "/home/lars/etc/contacts.turtle" >>= (\g -> runUi g)-- return.emailStringList >>= print




main:: IO ()
main = -- runO $
       do
         args <- observe "Arguments" getArgs 
         let (opts, nonOpts) = case getOpt Permute mainoptions args of
               (o,n,[])   -> (observe "Opts" $ foldl (flip id) defaultOptions o, observe "nonOpts" n)
               (_,_,errs) -> error ("Error " ++ (concat errs ++ usageInfo "Usage: " mainoptions))
             (Just action) = lookupCmd commands $ case nonOpts of
               []    -> error "no command given"
               (c:_) -> observe "command given" c
         bg <- bibtexGraphs    $ observe "BibTex input" $ inputBibTex opts
         lg <- parseMergeLocal $ observe "inputGraphs" $ inputGraphs opts
         action (mergeGraphs [bg, lg]) opts
       where
         bibtexGraphs bf = (sequence $ map (parseBibTeX) bf)
                           >>= (\x -> (return.bibTeXToRDF.concat) x:: IO R.TriplesGraph)
                           
         
