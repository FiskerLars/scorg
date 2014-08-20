module TextUi (runTextUI) where

import Control.Monad.State

import UI.HSCurses.Logging
import UI.HSCurses.Widgets
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

import SCState

type SC = SCState IO


styles:: [CursesH.Style]
styles = [ CursesH.defaultStyle
         , CursesH.AttributeStyle [CursesH.Bold] CursesH.GreenF CursesH.DarkBlueB
         ]

{-| Resize Window
TODO: resize columns
-}
resize:: Widget w => SC w -> SC ()
resize f = do liftIO $ do Curses.endWin
              Curses.resetParams
              Curses.cursSet Curses.CursorInvisible
              Curses.refresh


redraw:: Widget w => w -> SC ()
redraw = undefined


{-| Main Window Keyboard Listener
-}
eventloop w = do k <- CursesH.getKey (resize mkMainWidget)
                 case k of
                   Curses.KeyChar 'q' -> return ()
                   _ -> eventloop w

data ToplineWidget = ToplineWidget
                     { topline:: String
                     }
mkToplineWidget = do return $ ToplineWidget "topline"

data BottomlineWidget = BottomlineWidget
                     { bottomline:: String
                     }
mkBottomlineWidget = do return $ BottomlineWidget "bottomline"

data ColumnWidget = ColumnWidget
                    { entries :: [String]
                    }
mkColumnWidget l = do return $ ColumnWidget l

data MainWidget = MainWidget
                  { toplineWidget :: ToplineWidget
                  , bottomlineWidget :: BottomlineWidget
                  , columnAWidget :: ColumnWidget
                  , columnBWidget :: ColumnWidget
                  , columnCWidget :: ColumnWidget
                  }
mkMainWidget = do
  tlw <- mkToplineWidget
  blw <- mkBottomlineWidget
  colA <- mkColumnWidget [] 
  colB <- mkColumnWidget [] -- TODO: Init with all subjects/or initial query
  colC <- mkColumnWidget [] 
  return $ MainWidget tlw blw colA colB colC

runUi = do w <- mkMainWidget
           redraw w
           eventloop w
              


runTextUI:: IO ()
runTextUI = do CursesH.start() -- :: IO ()
               cstyles <- CursesH.convertStyles styles
               Curses.cursSet Curses.CursorInvisible
               