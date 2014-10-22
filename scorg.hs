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

import Data.List
import Data.RDF as R
import Data.RDF.Query as Q
import qualified Data.Text as T
import System.Environment

import Debug.Trace
import Debug.Hood.Observe

import TextUi
import BibTeXInput
import LatexOutput

instance Observable R.TriplesGraph where
  observer = observeBase


{-| Parser for turtle file int TriplesGraph
-}
parseLocal:: String -> IO R.TriplesGraph
parseLocal file = R.parseFile (R.TurtleParser Nothing Nothing) file >>= (\r -> 
                                              case r of
                                                (Left _) -> return R.empty
                                                (Right rdf) -> return rdf )
                                                               

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







latexcv:: Worker
latexcv opts = parseLocal (filename opts)
               >>= return.((flip genCvLatex)(R.unode $ T.pack "#me"))
               >>= putStrLn
  where
    filename (f:opts) = f

latexlectures:: Worker
latexlectures opts = observe "latexlectures" parseLocal (filename opts)
                     >>= trace "lectures" return
                     >>= return.((flip genericTeachingList) defaultCourses)
                     >>= putStrLn
  where
    filename (f:opts) = f


type Options = [String]
type Worker = Options -> IO () 
data Command = Command String Worker String
commands:: [Command]
commands = [ Command "cvlatex" latexcv
             "Generate a CV of me in LaTeX"
           , Command "lectureslatex" latexlectures
           "Only the lectures part"]

  
{- lookup function matching cmd in List of commands -}
lookupCmd:: [Command] -> String -> Maybe Worker
lookupCmd cmds s = let
  cmdName (Command cmd _ _) = cmd
  in case (find (\c -> isPrefixOf s $ cmdName c) cmds) of
    (Just (Command _ w _)) -> Just w
    (Nothing)              -> Nothing



 

mainUi:: IO ()                     
mainUi = parseLocal "/home/lars/etc/contacts.turtle" >>= (\g -> runUi g)-- return.emailStringList >>= print


main:: IO ()
main = runO $ getArgs >>= cmdSelector 
       where
         cmdSelector:: Options -> IO ()
         cmdSelector [] = mainUi
         cmdSelector (cmd:args) = case lookupCmd commands cmd of
           Nothing  -> error $ "Unknown cmd " ++ cmd
           (Just w) -> w args

