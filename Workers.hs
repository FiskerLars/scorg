module Workers where

import Data.List

import Data.RDF as R
import Data.RDF.CV

import qualified Data.Text as T

import LatexOutput
import LatexConfig
import Options
import Data.RDF.GraphDB (Graph)
---------------------------------------------------------------------------------
-- Workers
type Worker = Graph-> Options -> IO () 


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


mainUi:: IO ()                     
mainUi = undefined -- parseLocal "/home/lars/etc/contacts.turtle" >>= (\g -> runUi g)-- return.emailStringList >>= print



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

