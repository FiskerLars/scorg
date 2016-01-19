module CLIarguments where

import LanguageSelector
import Workers
import System.Environment
import System.Console.GetOpt



import Options

type ArgStr = [String]



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



parseArgs:: IO (Worker, Options, [String])
parseArgs = getArgs >>=
            (\args ->
              let (opts, nonOpts) = case getOpt Permute mainoptions args of
                    (o,n,[])   -> ( foldl (flip id) defaultOptions o, n)
                    (_,_,errs) -> error ("Error " ++ (concat errs ++ usageInfo "Usage: " mainoptions))
                  (Just action) = lookupCmd commands $ case nonOpts of
                    []    -> "mainUi" --error "no command given"
                    (c:_) -> c
              in return (action, opts, nonOpts) 
            )

