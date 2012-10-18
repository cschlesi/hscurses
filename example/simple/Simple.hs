module Main where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import UI.HSCurses.Widgets
import Data.List

type Widgets = (TextWidget, TextWidget, TextWidget, TextWidget)

data MainState
  = TopWidget
  | MidWidget
  | BotWidget
  | CmdWidget

-- |Refresh the stdScr followed by each widget.  The widget in focus---as 
-- specified by MainState---will be drawn with focus.
refreshAll :: Widgets -> MainState -> IO ()
refreshAll (topW, midW, botW, cmdW) state = do
  topSize <- getMaxYX $ textWidgetGetWin topW
  midSize <- getMaxYX $ textWidgetGetWin midW
  botSize <- getMaxYX $ textWidgetGetWin botW
  cmdSize <- getMaxYX $ textWidgetGetWin cmdW
  let topHint = case state of { TopWidget -> DHFocus; _ -> DHNormal }
  let midHint = case state of { MidWidget -> DHFocus; _ -> DHNormal }
  let botHint = case state of { BotWidget -> DHFocus; _ -> DHNormal }
  let cmdHint = case state of { CmdWidget -> DHFocus; _ -> DHNormal }
  refresh
  draw (0,0) topSize topHint topW
  draw (0,0) midSize midHint midW
  draw (0,0) botSize botHint botW
  draw (0,0) cmdSize cmdHint cmdW
  wRefresh $ textWidgetGetWin topW
  wRefresh $ textWidgetGetWin midW
  wRefresh $ textWidgetGetWin botW
  wRefresh $ textWidgetGetWin cmdW

mainEventLoop :: Widgets -> MainState -> IO ()
mainEventLoop widgets@(topW, midW, botW, cmdW) state = do
  -- Refresh everything.
  refreshAll widgets state

  -- Get the next input.
  k <- getCh

  -- Handle common input, like ^w.
  case k of
    KeyChar c
      | c == '\ETB' -> do
          newState <- shiftFocus state
          mainEventLoop widgets newState
      | otherwise -> return ()
    _ -> return ()

  -- Otherwise, handle input based on the active widget.
  newWidgets <- case state of
    TopWidget -> handleTopKey widgets k
    MidWidget -> handleMidKey widgets k
    BotWidget -> handleBotKey widgets k
    CmdWidget -> handleCmdKey widgets k

  mainEventLoop newWidgets state

-- |Check for 'j' or 'k' after receiving '^w' to effect a focus shift.  Ignore
-- any other keystroke.
shiftFocus :: MainState -> IO MainState
shiftFocus state = do
  k <- getCh
  case (k,state) of
    (KeyChar c, TopWidget)
      | c == 'k' -> return TopWidget
      | c == 'j' -> return MidWidget
    (KeyChar c, MidWidget)
      | c == 'k' -> return TopWidget
      | c == 'j' -> return BotWidget
    (KeyChar c, BotWidget)
      | c == 'k' -> return MidWidget
      | c == 'j' -> return CmdWidget
    (KeyChar c, CmdWidget)
      | c == 'k' -> return BotWidget
      | c == 'j' -> return CmdWidget
    (_,_) -> return state
    

handleTopKey :: Widgets -> Key -> IO (Widgets)
handleTopKey widgets@(topW, midW, botW, cmdW) key = do
  topW <- doScroll key topW
  return (topW, midW, botW, cmdW)

handleMidKey :: Widgets -> Key -> IO (Widgets)
handleMidKey widgets@(topW, midW, botW, cmdW) key = do
  midW <- doScroll key midW
  return (topW, midW, botW, cmdW)

handleBotKey :: Widgets -> Key -> IO (Widgets)
handleBotKey widgets@(topW, midW, botW, cmdW) key = do
  botW <- doScroll key botW
  return (topW, midW, botW, cmdW)

handleCmdKey :: Widgets -> Key -> IO (Widgets)
handleCmdKey widgets@(topW, midW, botW, cmdW) key = do
  cmdW <- doScroll key cmdW
  return (topW, midW, botW, cmdW)


-- |If key is a scroll key, return an updated widget.  Otherwise, return the 
-- original.
doScroll :: Key -> TextWidget -> IO (TextWidget)
doScroll key tw = do
  size <- getMaxYX $ textWidgetGetWin tw
  let tw' = case key of
              KeyChar c
                | c == 'j' -> textWidgetScrollDown size tw
                | c == 'k' -> textWidgetScrollUp size tw
                | c == 'l' -> textWidgetScrollRight size tw
                | c == 'h' -> textWidgetScrollLeft size tw
              KeyDown      -> textWidgetScrollDown size tw
              KeyUp        -> textWidgetScrollUp size tw
              KeyRight     -> textWidgetScrollRight size tw
              KeyLeft      -> textWidgetScrollLeft size tw
              _            -> tw
  return tw'

initializeWidgets :: MainState -> IO Widgets
initializeWidgets state = do
  -- Divide the screen.
  (height',width') <- scrSize
  let height = height' - 1
  let width = width' - 1
  let lines = 3
  let topHeight = 2
  let cmdHeight = 2
  let midHeight = floor $ (fromIntegral (height - topHeight - cmdHeight - lines)) * 0.2
  let botHeight = height - topHeight - midHeight - cmdHeight - lines

  -- The window is only 1 high, but reserve some whitespace for aesthetics.
  topWin <- newWin 1 width 1 0
  let topW = newWTextWidget topWin defaultTWOptions textWide

  move topHeight 0
  drawLine width $ repeat '-'

  let midWinY = topHeight + 1
  midWin <- newWin midHeight width midWinY 0
  let midW = newWTextWidget midWin defaultTWOptions textWide

  move (midWinY + midHeight) 0
  drawLine width $ repeat '-'

  let botWinY = midWinY + midHeight + 1
  botWin <- newWin botHeight width botWinY 0
  let botW = newWTextWidget botWin defaultTWOptions textLong

  move (botWinY + botHeight) 0
  drawLine width $ repeat '-'

  -- Again, add some whitespace for aesthetics.
  let cmdWinY = botWinY + botHeight + 2
  cmdWin <- newWin 1 width cmdWinY 0
  let cmdW = newWTextWidget cmdWin defaultTWOptions textWide

  let widgets = (topW, midW, botW, cmdW)
  refreshAll widgets state
  return widgets

textLong = concat . intersperse "\n" $ map show [1..200]
textWide = concat . intersperse " " $ map show [1..200]

main = do
  -- Initialize.
  initCurses
  cBreak True
  keypad stdScr True
  echo False
  clear
  refresh

  let startState = CmdWidget
  widgets <- initializeWidgets startState
  mainEventLoop widgets startState

  endWin

