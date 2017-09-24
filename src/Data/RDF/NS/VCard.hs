module Data.RDF.NS.VCard where

import qualified Data.RDF as R

import Data.Maybe
import Debug.Trace

import RdfHandler


vcardStreetAddrView g vc = mHeadObjView g "Addr missing" vc (Just vcStreetAddress)
vcPostalCodeView g vc    = mHeadObjView g "postal code missing" vc (Just vcPostalCode)
vcLocalityView g vc      = mHeadObjView g "locality missing" vc (Just vcLocatity)
vcCellphoneView:: RDFdb -> [R.Subject] -> String
vcCellphoneView g = (fromMaybe "Cellphone missing")
                    .(\n -> trace ("view Phone " ++ (show n)) n)
                    .(\l -> listToMaybe l
                            >>= (listToMaybe.(vcHasValueObj g))
                            >>= return.(R.view))
                    .(\n -> trace ("is Cell Type " ++ (show n)) n)

                          
{-| remove all non-cellphones from list |-}
filterVcCells:: RDFdb -> [R.Subject] -> [R.Subject]
filterVcCells g = filter (isVcCellType g)

vcHasValueObj g sub = queryObjects g sub vcHasValue
mvcPhoneNodes  g me = mQueryObjects g (Just me) (Just vcHasTelephone)

{-| Returns True if the given phone is known as a cellphone
-}
isVcCellType:: RDFdb -> R.Subject -> Bool
isVcCellType g phone = not.null $ R.query g (Just phone) (Just typePred) (Just vcCellType)

vcardHasAddressPred:: RDFdb -> R.Subject -> [R.Object]
vcardHasAddressPred = objectsByPred (mkUnode' vcard "hasAddress")


vcStreetAddress = mkUnode' vcard "street-address"
vcPostalCode = mkUnode' vcard "postal-code"
vcLocatity = mkUnode' vcard "locality"
vcCellType = mkUnode' vcard "Cell"
vcHasTelephone = mkUnode' vcard "hasTelephone"
vcHasValue = mkUnode' vcard "hasValue"
