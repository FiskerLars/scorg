module Data.RDF.NS.VCard where

import qualified Data.RDF as R

import Data.Maybe
import RdfHandler

vcardStreetAddrView g vc = mHeadObjView g "Addr missing" vc (Just vcStreetAddress)
vcPostalCodeView g vc    = mHeadObjView g "postal code missing" vc (Just vcPostalCode)
vcLocalityView g vc      = mHeadObjView g "locality missing" vc (Just vcLocatity)
vcCellphoneView g vc     = fromMaybe "Cellphone missing"
                           $ (\l -> listToMaybe l >>= (listToMaybe.(vcHasValueObj g)) >>= return.(R.view))
                           $ filter  (isVcCellType g)
                           $ mvcPhoneNodes g

vcHasValueObj g sub = queryObjects g sub vcHasValue
mvcPhoneNodes  g = mQueryObjects g (Just meNode) (Just vcHasTelephone)

{-| Returns True if the given phone is known as a cellphone
-}
isVcCellType:: R.RDF a => a -> R.Subject -> Bool
isVcCellType g phone = not.null $ R.query g (Just phone) (Just typePred) (Just vcCellType)

vcardHasAddressPred:: R.RDF a => a -> R.Subject -> [R.Object]
vcardHasAddressPred = objectsByPred (mkUnode' vcard "hasAddress")


vcStreetAddress = mkUnode' vcard "street-address"
vcPostalCode = mkUnode' vcard "postal-code"
vcLocatity = mkUnode' vcard "locality"
vcCellType = mkUnode' vcard "Cell"
vcHasTelephone = mkUnode' vcard "hasTelephone"
vcHasValue = mkUnode' vcard "hasValue"
