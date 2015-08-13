{-| Parse publications from my repos

primary use is to generate my webpage/CV

- Bibtex
-}
module BibTeXInput ( parseBibTeX
                   , bibTeXToRDF
                    )  where


import Prelude hiding (readFile)
import qualified Text.BibTeX.Parse as Parse
import qualified Text.BibTeX.Entry as Entry
import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Data.Text as T
import qualified Data.RDF as R
import qualified Data.RDF.Namespace as N
import qualified Data.Map as M
import Data.Char


import Control.Exception.Base
import Control.Monad
import System.IO

import Data.RDF.Bibliography
import RdfHandler


{-
https://code.google.com/p/bibotools/source/browse/bibo-ontology/tags/1.0/bibo.n3

bibo:AcademicArticle
-}
bib2RDFmap:: M.Map String (R.Subject -> T.Text -> R.Triples)
bib2RDFmap = M.fromList [  ("author", authorSplitGen authorPred)
                         , ("title", singleton title )
                         , ("publisher", singleton publisher)
                         , ("year", singleton year)
                         , ("booktitle", singleton booktitle)
                         , ("journal", singleton booktitle)
                         , ("file", (\s t -> map (\(f,_) -> R.triple s filePred (R.unode $ f))
                                             $ splitFiles t))
                         , ("pdfurl", singleton pdfurl)
                         , ("url", singleton url)
                         , ("pages", pages)
                         , ("editor", authorSplitGen editorPred)
                         , ("timestamp", singleton dbDateInserted)  
                         ]
             where singleton:: (R.Subject -> T.Text -> R.Triple) -> R.Subject -> T.Text -> R.Triples
                   singleton f s t = f s t:[]
                   authorSplitGen p s t = map ((literalTriple s p).(T.pack).(\a -> a)) $ Parse.splitAuthorList$ T.unpack t
                   splitFiles:: T.Text -> [(T.Text,T.Text)] -- returns (file,type)
                   splitFiles = pairwise.(T.splitOn (T.pack ":"))
                   pairwise [] = []
                   pairwise (a:[]) = []
                   pairwise (a:b:[]) = (a,b):[]
                   pairwise (a:b:xs) = (a,b):pairwise xs
                   

{-| Derive triples from known attributes in BibTex Entry
-}
mapEntryOntoSubj:: R.Subject -> Entry.T -> R.Triples
mapEntryOntoSubj s e = case sequence
                            $ filter (/= Nothing) -- not nice, but works
                            $ map (\(k,v) ->
                                    M.lookup (map toLower k) bib2RDFmap >>= (\f -> (Just $ f s (T.pack v))))
                            (Entry.fields e) of
                         Nothing -> []
                         (Just l) -> concat l
                         






{-| generate a graph (maybe even collate authors)
TODO: differentiate different classes of publications
-}
bibTeXToRDF:: R.RDF a => [Entry.T] -> a
bibTeXToRDF entries = pubGraph $ foldr (\e t ->  newPublication e ++ t) [] entries 
  where
    newPublication e = R.triple (publicationNode (Entry.identifier e))  
                       typePred
                       (strRnode "bibo:Article") 
                       : (mapEntryOntoSubj (publicationNode (Entry.identifier e)) e)
    
 
{-| parse a BiBTeχ File into a List of Entry.T(ypes)
TODO: handle @String
TODO: test handling of JabRef Structures
TODO: read directly from handle, apply on handle h: hSetEncoding h utf8
-}
parseBibTeX:: String -> IO [Entry.T]
parseBibTeX fname = readFile fname >>=
                  {- bracket -- too lazy?
                  (openFile fname ReadMode)
                  hClose
                  ( hGetContents 
                    >=> (\str -> return$ (trace (str) str))
                    >=> -}
                    (\str -> return $ case parseFile fname str of
                        (Left e)  -> error $ show e
                        (Right l) -> l
                    )
                  
  where
    --parseFile _ s = Right s :: Either String String
    parseFile = Parsec.parse (Parsec.skipMany Parsec.space >> Parse.file) 
