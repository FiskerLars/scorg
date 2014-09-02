{-# LANGUAGE ScopedTypeVariables,
OverloadedStrings, Rank2Types #-}

{-|
-}
module TextUi (runUi) where

import Data.List

import Control.Monad.State
import Control.Exception


import UI.HSCurses.Logging
import UI.HSCurses.Widgets
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

import Data.RDF as R
import qualified Data.Text as T


{-| Current internal state
-}
data SCState = SCState
               { graph :: R.TriplesGraph -- RDF r => r
               , sc_styles:: [CursesH.CursesStyle]
               }

{-
StateT :: (s -> m (a, s)) -> StateT s m a
SC :: a -> StateT SCState IO a 
-}
type SC = StateT SCState IO 



{- TODO: use a map for more descriptive style indicees -}
styles:: [CursesH.Style]
styles = [ CursesH.defaultStyle  
         , CursesH.AttributeStyle [CursesH.Bold] CursesH.GreenF CursesH.DarkBlueB -- Highlight style
         ]

{-| Resize Window
TODO: resize columns
-}
--resize:: Widget w => w -> SC ()
resize f = do liftIO $ do Curses.endWin
                          Curses.resetParams
                          Curses.cursSet Curses.CursorInvisible
                          Curses.refresh


redraw:: Widget w => w -> SC ()
redraw w = do sz <- getSize
              liftIO $ draw (0, 0) (getHeight sz , getWidth sz -1 ) DHNormal w -- FIXME: Why o Why do I need -1
              liftIO $ Curses.refresh
              

getSize:: MonadIO m => m Size
getSize = liftIO $ Curses.scrSize

nthStyle :: Int -> SC CursesH.CursesStyle
nthStyle n =  gets sc_styles >>= (\cs -> return $ cs !! n)

lineStyle = nthStyle 1
textBoxStyle = nthStyle 0

lineDrawingStyle = lineStyle    >>= return.mkDrawingStyle
textDrawingStyle = textBoxStyle >>= return.mkDrawingStyle

textFillOptions =
  do sz <- getSize
     ds <- textDrawingStyle
     return $ TWOptions { twopt_size = TWSizeFixed $ trace (show sz) sz, --(getHeight sz, getWidth sz),
                          twopt_style = ds,
                          twopt_halign = AlignLeft }
lineOptions =
    do sz <- getSize
       ds <- lineDrawingStyle
       return $ TWOptions { twopt_size = TWSizeFixed (1, getWidth sz - 1),
                            twopt_style = ds,
                            twopt_halign = AlignLeft }


{-| Main Window Keyboard Listener
-}
--eventloop:: Widget w => SC w -> SC ()
eventloop w = do k <- CursesH.getKey (resize mkMainWidget)
                 case k of
                   Curses.KeyChar 'q' -> return ()
                   _ -> eventloop w




{- TODO make top and bottomline into lines with contents for left, right and centre (similar to fancyhdr in LaTeχ)
-}
type ToplineWidget = TextWidget
type BottomlineWidget = TextWidget



     
mkToplineWidget =
  do opts <- lineOptions
     return $ newTextWidget (opts { twopt_halign = AlignLeft }) "Topline"
mkBottomlineWidget =
  do opts <- lineOptions
     return $ newTextWidget (opts { twopt_halign = AlignRight }) "Bottomline" 



{-| Column List Widget
    Depicts a column of linewise browsable Entries
-}
data ListWidget = ListWidget
                    { entries :: [String]
                    }
-- draw:: Pos -> Size -> DrawingHint -> a -> IO
instance Widget ListWidget where
  draw pos sz hint w = draw pos sz hint (mkRealListWidget (Just $ TWSizeFixed sz) w)
  minSize w = minSize (mkRealListWidget Nothing w)

mkListWidget l = do return $ ListWidget l

mkRealListWidget msz w =
  let opts = case msz of
        Nothing -> defaultTWOptions
        Just sz -> defaultTWOptions {twopt_size = sz } 
  in newTextWidget opts (intercalate "\n" (entries w)) 


{-| BrowserWidget provides the main view consisting of 3 Lists/Columns
-}
data BrowserWidget = BrowserWidget
                     { left:: ListWidget
                     , centre:: ListWidget
                     , right:: ListWidget
                     }
instance Widget BrowserWidget where
  draw pos sz hint w = draw pos sz hint (mkRealBrowserWidget (Just sz) w)
  minSize w = minSize (mkRealBrowserWidget Nothing w)

mkBrowserWidget:: SC BrowserWidget
mkBrowserWidget = do
  l <- mkListWidget ["Left1", "Left2"]
  c <- mkListWidget ["Centre1", "Centre2", "Centre3"]
  r <- mkListWidget ["Right1"]
  return $ BrowserWidget l c r
                           
mkRealBrowserWidget msz b =
  let row = [ TableCell $ left b
            , TableCell $ centre b
            , TableCell $ right b ]
      opts = case msz of
        Nothing -> defaultTBWOptions
        Just sz -> defaultTBWOptions { tbwopt_minSize = sz }
  in newTableWidget opts [row]

{-  let opts = textFillOptions -- TODO: Create columnOpts
                     in newTextWidget (opts {twopt_halign = AlignLeft})
                                               (intercalate "\n" (entries w))
-}
-------------------------------------------------------------------------------------------

data MainWidget = MainWidget
                  { topline :: ToplineWidget
                  , bottomline :: BottomlineWidget
                  , browser :: BrowserWidget
                  }
instance Widget MainWidget where
  draw pos sz hint w = draw pos sz hint (mkRealMainWidget (Just sz) w)
  minSize w = minSize (mkRealMainWidget Nothing w)

mkMainWidget:: SC MainWidget
mkMainWidget = do
  tlw <- mkToplineWidget
  blw <- mkBottomlineWidget
  bro <- mkBrowserWidget
  return $ MainWidget tlw blw bro

mkRealMainWidget msz w =
  let cells = [ TableCell $ topline w
             , TableCell $ browser w
             , TableCell $ bottomline w
             ]
      rows = map singletonRow cells
      opts = case msz of
        Nothing -> defaultTBWOptions 
        Just sz -> defaultTBWOptions { tbwopt_minSize = sz
                                     , tbwopt_fillCol = Just 2
                                     , tbwopt_fillRow = None}
  in newTableWidget opts rows





-- mkMainWidget = textFillOptions >>= (return.(flip newTextWidget) "Main Window")
--  do opts <- textFillOptions
--                  return $ newTextWidget defaultTWOptions "Main Window" 

--  do liftIO $ (Curses.scrSize) >>= return.EmptyWidget


textUiMain:: SC ()
textUiMain = do w <- mkMainWidget
                redraw w
                eventloop w
                return ()

{- if called by runUi it should be -> IO ()
-}
runSC :: [CursesH.CursesStyle] -> SCState -> SC a -> IO a
runSC styles state sc = evalStateT sc state


--runUi::  RDF r => r ->  IO ()
runUi gr = do runit gr `finally` CursesH.end
  where
    --    runit:: RDF r => r -> IO ()
    runit r =
      do CursesH.start -- :: IO ()
         cstyles <- CursesH.convertStyles styles
         Curses.cursSet Curses.CursorInvisible
         runSC cstyles (SCState r cstyles) textUiMain
                      
