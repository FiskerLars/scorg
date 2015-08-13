module AcademicTerm where

import Text.ParserCombinators.Parsec
-- import Debug.Hood.Observe

data AcademicTerm = Term WinterSummer Integer deriving Eq
data WinterSummer = Winter | Summer deriving Eq

instance Ord WinterSummer where
  compare Winter Summer = GT
  compare Summer Winter = LT
  compare _ _           = EQ



instance Ord AcademicTerm where
  compare (Term ws year) (Term ws' year') = case compare year year' of
                                              EQ -> compare ws ws'
                                              x  -> x

instance Read WinterSummer where
  readsPrec _ s = case splitAt 2 s of
    ("ws",s') -> [(Winter, s')]
    ("ss",s') -> [(Summer, s')]
--    (_, _) -> error
    


parserWS = try winter <|> summer
           where
             winter = char 'w' >> uptoNum
             summer = char 's' >> uptoNum
             uptoNum = skipMany letter
-- parserYear = many1 digit

instance Read AcademicTerm where
  -- result String -> [(a, String)]
  readsPrec d r = (\r -> [(Term t y, rest) |
                           (t, s) <- readsPrec d r,
                           (y, rest ) <- readsPrec d s]) r
                
