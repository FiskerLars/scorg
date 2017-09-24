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
import RdfHandler

import Debug.Trace
-- import Debug.Hood.Observe

-- import TextUi
import BibTeXInput
import LatexCV
import LatexConfig
import LanguageSelector
import TextUi

import Data.RDF.CV
-- import Database.HSparql.QueryGenerator

type Graph = RDFdb


testding:: RDFdb -> String
testding g = "<test> <has> <tist>"


{-instance Observable R.TriplesGraph where
  observer = observeBase
-}

{-| Parser for turtle file int TriplesGraph
-}
parseLocal:: String -> IO RDFdb
parseLocal file = (R.parseFile (R.TurtleParser Nothing Nothing) file)
                  >>= (\r -> 
                        case r of
                          (Left err ) -> error $ "Could not parse " ++ (file ++ (show err))-- return R.empty
                          (Right rdf) -> return rdf )
                                                               


parseMergeLocal:: [String] -> IO RDFdb
parseMergeLocal [] = return (empty ::RDFdb) 
parseMergeLocal gs = (sequence $  map (parseLocal) gs)
                     >>= return.mergeGraphs


mergeGraphs gs =  (\(m,t) -> R.mkRdf t Nothing m)
                  $ foldl (\(maps,triples) g -> (N.mergePrefixMappings maps (R.prefixMappings g)
                                                 ,R.triplesOf g ++ triples )) (N.ns_mappings [],[]) gs 

    
persons:: RDFdb -> [R.Subject]
persons g = map Q.subjectOf
            $ query g (Nothing)
                      (Just $ unode "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                      (Just $ unode "http://xmlns.com/foaf/spec/Person")  -- FIXME foldl into difflist (remove Nothings, eventually return empty list)


{- List all Persons (by name) and related email addresses -}
emailStringList:: RDFdb -> [(String,[String])]
emailStringList gr = map (\s -> (show $ objectOf $ head $ query gr (Just s) (Just $ unode "http://xmlns.com/foaf/spec/name") Nothing,
                                 map (show.objectOf) $ query gr (Just s) (Just $ unode "http://xmlns.com/foaf/spec/mbox") Nothing))
                     $ persons gr





---------------------------------------------------------------------------------
-- Workers

type Worker = Graph-> Options -> IO () 

--parseBibTeX:: String -> IO [Entry.T]
--bibTeXToRDF:: R.RDF a => [Entry.T] -> a

printGraph::Worker
printGraph g _ = putStrLn $ show g

rdfCv:: Worker
rdfCv g o = either (printErr) ( putStrLn.show.(cvParts g).(R.unode).(T.pack) )
  $ rootSubject o
  

latexcv:: Worker
latexcv g o = either
  printErr 
  (\s -> (flip.flip genCvLatex) (R.unode $ T.pack s) (genLatexConfig o) g >>= putStrLn )
  (rootSubject o)

latexlectures:: Worker
latexlectures g = (either
                   printErr
                   (\s -> putStrLn $ ((flip genericTeachingList)
                                      $ allCourses g (R.unode $ T.pack s) 
                                     ) g)
                  ).rootSubject
genLatexConfig:: Options -> LatexConfig
genLatexConfig o = LatexConfig {lang = language o}

data Command = Command String Worker String
commands:: [Command]
commands = [ Command "cvlatex" latexcv
             "Generate a CV of <http://larsipulami.de/me> in LaTeX"
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
           , Command "gui" (\_ _ -> mainUi)
             "Run the main ui" 
           , Command "help"(\_ _ -> printUsage)
             "Show help text"
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
     , outputType:: String
     , rootSubject:: Either String String 
     } deriving Show

{-
instance Observable Options where
  observer = observeBase
-}

printErr:: String -> IO ()
printErr = (>> printUsage).putStrLn

defaultOptions = Options
     { inputGraphs = []
     , inputBibTex = []
     , language = EN
     , outputType = "rdf"
     , rootSubject = Left "no Subject URL given" -- R.unode $ T.pack "http://larsipulami.de/lars_fischer#me"
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
  , Option ['o'] ["output"] (ReqArg (\s o -> o { outputType = s })
                             "output type")
    "Select an output type."
  , Option ['s'] ["subject"] (OptArg (\s o -> o { rootSubject = maybe (rootSubject defaultOptions) (Right) s})
                             "IRI")
    "Subject used as reference root for some operations, e.g. Person to generate a CV of, point of view of operations"
  ]


--- Startups


mainUi:: IO ()                     
mainUi =  parseFile (TurtleParser Nothing Nothing) "/home/lars/etc/contacts.turtle" >>= either (putStrLn.show) (\g -> runUi g)-- return.emailStringList >>= print



printUsage:: IO ()
printUsage = putStrLn (usageInfo "Usage: " mainoptions)

main:: IO ()
main = 
       do
         args <-  getArgs 
         let (opts, nonOpts) = case getOpt Permute mainoptions args of
               (o, n, [])   -> ( foldl (flip id) defaultOptions o, n)
               (_, _, errs) -> error ("Error " ++ (concat errs ++ usageInfo "Usage: " mainoptions))
             (Just action) = lookupCmd commands $ case nonOpts of
               []    -> error "no command given"
               (c:_) -> c
         bg <- bibtexGraphs    $ inputBibTex opts
         lg <- parseMergeLocal $ inputGraphs opts
         action (mergeGraphs [bg, lg]) opts
       where
         bibtexGraphs bf = (sequence $ map (parseBibTeX) bf)
                           >>= (\x -> (return.bibTeXToRDF.concat) x:: IO RDFdb)
                           
         
