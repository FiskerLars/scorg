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




styles:: [CursesH.Style]
styles = [ CursesH.defaultStyle
         , CursesH.AttributeStyle [CursesH.Bold] CursesH.GreenF CursesH.DarkBlueB
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
              liftIO $ draw (0, 0) (getHeight sz , getWidth sz -1 ) DHNormal w
              liftIO $ Curses.refresh
              

getSize:: MonadIO m => m Size
getSize = liftIO $ Curses.scrSize

nthStyle :: Int -> SC CursesH.CursesStyle
nthStyle n =  gets sc_styles >>= (\cs -> return $ cs !! n)

lineStyle = nthStyle 1
textBoxStyle = nthStyle 1

lineDrawingStyle = lineStyle    >>= return.mkDrawingStyle
textDrawingStyle = textBoxStyle >>= return.mkDrawingStyle

textFillOptions =
  do sz <- getSize
     ds <- textDrawingStyle
     return $ TWOptions { twopt_size = TWSizeFixed $ trace (show sz) sz, --(getHeight sz, getWidth sz),
                          twopt_style = ds,
                          twopt_halign = AlignLeft }


{-| Main Window Keyboard Listener
-}
--eventloop:: Widget w => SC w -> SC ()
eventloop w = do k <- CursesH.getKey (resize mkMainWidget)
                 case k of
                   Curses.KeyChar 'q' -> return ()
                   _ -> eventloop w


{-

{- TODO make top and bottomline into lines with contents for left, right and centre (similar to fancyhdr in LaTeχ)
-}
type ToplineWidget = TextWidget
type BottomlineWidget = TextWidget



     
mkToplineWidget = do opts <- lineOptions
                     return $ newTextWidget (opts { twopt_halign = AlignCenter }) "title"
mkBottomlineWidget = do opts <- lineOptions
                        return $ newTextWidget (opts { twopt_halign = AlignLeft }) "title" 

{-
data ColumnWidget = ColumnWidget
                    { entries :: [String]
                    }
                    
instance Widget ColumnWidget where
  draw pos sz hint w = draw pos sz hint (mkRealColumnWidget w)
  minSize w = minSize (mkRealColumnWidget w)

mkColumnWidget l = do return $ ColumnWidget l

mkRealColumnWidget w = do opts <- lineOptions -- TODO: Create columnOpts
                          return $ newTextWidget (opts {twopt_halign = AlignLeft})
                            (intercalate "\n" (entries w))
-}

                
type ColumnWidget = EmptyWidget

mkColumnWidget = EmptyWidget ((getHeight getSize), (getWidth getSize) `div` 3)

data MainWidget = MainWidget
                  { toplineWidget :: ToplineWidget
                  , bottomlineWidget :: BottomlineWidget
                  , columnAWidget :: ColumnWidget
                  , columnBWidget :: ColumnWidget
                  , columnCWidget :: ColumnWidget
                  }
instance Widget MainWidget where
  draw pos sz hint w = draw pos sz hint (mkRealMainWidget (Just sz) w)
  minSize w = minSize (mkRealMainWidget Nothing w)

mkMainWidget:: SC MainWidget
mkMainWidget = do
  tlw <- mkToplineWidget
  blw <- mkBottomlineWidget
  colA <- mkColumnWidget [] 
  colB <- mkColumnWidget [] -- TODO: Init with all subjects/or initial query
  colC <- mkColumnWidget [] 
  return $ MainWidget tlw blw colA colB colC

mkRealMainWidget msz w =
  let rows = [ [ TableCell $ toplineWidget w
               ]
             , [ TableCell $ columnAWidget w
               , TableCell $ columnBWidget w
               , TableCell $ columnCWidget w
               ]
             , [ TableCell $ bottomlineWidget w
               ]
             ]
--      rows = map singletonRow cells
      opts = case msz of
        Nothing -> defaultTBWOptions
        Just sz -> defaultTBWOptions { tbwopt_minSize = sz }
  in newTableWidget opts rows

-}



mkMainWidget:: SC TextWidget
mkMainWidget = textFillOptions >>= return.(flip newTextWidget) "Main Window"
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
                      
