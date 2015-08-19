module Data.RDF.NS.Foaf where

import RdfHandler
import qualified Data.RDF.Namespace as N


foaf = N.foaf

{-| Functions to derive Objects Strings
-}
{-|
-}
foafTitleView g = headObjViewMod g "" (\s -> s++"\\\\\n") meNode foafTitle 
foafNameView g = headObjView g "Name of Subject Missing" meNode foafName
foafMboxView g = headObjView g "Mailaddr missing" meNode foafMbox
foafHomepageView g = headObjView g "Homepage missing" meNode foafHomepage


{-| Relation Nodes
-}
foafTitle = mkUnode' foaf "title"
foafName =  mkUnode' foaf "name"
foafMbox = mkUnode' foaf "mbox"
foafHomepage = mkUnode' foaf "homepage"
