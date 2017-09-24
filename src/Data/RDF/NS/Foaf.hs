module Data.RDF.NS.Foaf where

import RdfHandler
import qualified Data.RDF.Namespace as N


foaf = N.foaf

{-| Functions to derive Objects Strings
-}
{-|
-}
foafTitleView g s = headObjViewMod g "" (\s -> s++"\\\\\n") s foafTitle 
foafNameView g s = headObjView g "Name of Subject Missing" s foafName
foafMboxView g s = headObjView g "Mailaddr missing" s foafMbox
foafHomepageView g s = headObjView g "Homepage missing" s foafHomepage


{-| Relation Nodes
-}
foafTitle = mkUnode' foaf "title"
foafName =  mkUnode' foaf "name"
foafMbox = mkUnode' foaf "mbox"
foafHomepage = mkUnode' foaf "homepage"
