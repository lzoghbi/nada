{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada.App
    ( nadaApp
    ) where

import Nada.Types
import Nada.Calendar

import Data.List (intersperse)
import Data.Text (Text, pack, unlines, unwords, lines, isInfixOf)
import Data.Text.Zipper (textZipper)
import Data.Time
  ( formatTime
  , defaultTimeLocale
  , Day
  )
import Text.Wrap
  (defaultWrapSettings
  , preserveIndentation
  , breakLongWords
  , wrapText
  )

import Brick
import Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Edit as Ed
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

import Control.Monad.State
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as E

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.Platform()

-- FIXME: Separate this function from needing integers and use it with the
-- calendar.
drawTodoBare :: Integer -> Integer -> NadaState -> Widget Name
drawTodoBare iTodo jTodoList st =
  verticalB (thisTodo ^. todoPriority)
  <+>
  (padRight (Pad 1) (drawCompleted (thisTodo ^. todoCompleted))
    -- below the context of the editor is rendered, but the cursor is not visible yet - to be fixed soon
    -- if you want to see the cursor, but with no text rendering, uncomment the following
    -- <+> renderWrappedTxt ((Data.Text.unlines (Ed.getEditContents _todoName)))  
    <+> drawName
  )
  where
    thisTodoId = st ^?! visibleTodoLists.ix (fromInteger jTodoList).todoList.ix (fromInteger iTodo)
    thisTodo = st ^?! todosMap.ix thisTodoId
    thisTodoList = st ^?! visibleTodoLists.ix (fromInteger jTodoList)
    listIsSelected = jTodoList == st ^. selectedTodoList
    isSelectedTodo = listIsSelected && iTodo == thisTodoList ^. selectedTodo
    isEditingTodo = isSelectedTodo && (st ^. currentMode == ModeEdit)

    drawName = if isEditingTodo
                  then Ed.renderEditor (txt . Data.Text.unlines) True (st ^. todoEditor)
                  else txt (thisTodo ^. todoName)
    drawCompleted True  = txt "[X]"
    drawCompleted False = txt "[ ]"

-- drawTodo i j st draws the i-th Todo of the j-th TodoList in st
drawTodo :: Integer -> Integer -> NadaState -> Widget Name
drawTodo iTodo jTodoList st =
  withStyle $
    drawTodoBare iTodo jTodoList st
    <=> tags
    <=> dueDate
    <=> drawDescription
  where
    thisTodoId = st ^?! visibleTodoLists.ix (fromInteger jTodoList).todoList.ix (fromInteger iTodo)
    thisTodo = st ^?! todosMap.ix thisTodoId
    thisTodoList = st ^?! visibleTodoLists.ix (fromInteger jTodoList)
    listIsSelected = jTodoList == st ^. selectedTodoList
    isSelectedTodo = listIsSelected && iTodo == thisTodoList ^. selectedTodo
    isEditingTodo = isSelectedTodo && (st ^. currentMode == ModeEdit)
    withStyle
      | isEditingTodo = forceAttr editingAttr . visible
      | isSelectedTodo = forceAttr selectedAttr . visible
      | otherwise = id
    drawDescription = padLeft (Pad 6) $ renderWrappedTxt (thisTodo ^. todoDescription)
    tags = padLeft (Pad 4) $ (renderWrappedTxt . Data.Text.unwords) (thisTodo ^. todoTags)
    dueDate  = padLeft (Pad 4) $ hLimit 20 $ strWrapWith settings $
      case thisTodo ^. todoDueDate of
        Nothing -> ""
        Just d  -> "Deadline: " <> formatTime defaultTimeLocale "%Y-%d-%m" d
    settings = defaultWrapSettings { preserveIndentation = True
                                   , breakLongWords = True
                                   }
    

renderWrappedTxt :: Text -> Widget Name
renderWrappedTxt t = T.Widget T.Fixed T.Fixed
  $ do
    c <- T.getContext
    let w = c^.T.availWidthL
    T.render $
      (txt . wrapText settings w) t
      where
        settings = defaultWrapSettings { preserveIndentation = True
                                       , breakLongWords = True
                                       }

-- daysDiff:: Day -> IO Integer
-- daysDiff day = do
--   now <- getZonedTime
--   let today = localDay $ zonedTimeToLocalTime now
--   return (diffDays day today)

vborderMapping :: NadaPriority -> [(A.AttrName, V.Attr)]
vborderMapping priority =
  case priority of
    High   -> [(B.vBorderAttr, red `on` red)]
    Medium -> [(B.vBorderAttr, orange `on` orange)]
    Low    -> [(B.vBorderAttr, green `on` green)]
  where
    red    = V.rgbColor 255 65 55
    green  = V.rgbColor 119 221 119
    orange = V.rgbColor 255 150 79

verticalB :: NadaPriority -> Widget Name
verticalB priority =
  updateAttrMap (A.applyAttrMappings (vborderMapping priority)) $
  withBorderStyle BS.unicodeBold $
  vLimit 1 $
  B.vBorder

drawVisibleTodoLists :: NadaState -> Widget Name
drawVisibleTodoLists st = hBox $ intersperse B.vBorder drawnLists
  where
    drawnLists = do
      i <- toInteger <$> [0..(length (st ^. visibleTodoLists) - 1)]
      return $ drawTodoList st i

-- Draw the TodoList at the specified index
drawTodoList :: NadaState -> Integer -> Widget Name
drawTodoList st jTodoList = T.Widget T.Greedy T.Greedy $
  T.render $
    ( B.borderWithLabel (txt (tdl ^. todoListName))
    -- Currently no support for scrolling listst
    -- $ viewport NadaVP Vertical
    $ vBox
    $ intersperse B.hBorder drawnTodos
    )
  where
    tdl = st ^?! visibleTodoLists.ix (fromInteger jTodoList)
    drawnTodos = do
        iTodo <- [0..(length (tdl ^. todoList) - 1)]
        return $ drawTodo (toInteger iTodo) jTodoList st
  -- case _filterText `isInfixOf` (Data.Text.unlines . Ed.getEditContents . _todoName $ todo) of
  --   False -> []
  --   True  -> return $ withStyle $ drawTodo todo isFocused

-- Widget - Shortcut info
shortcutInfoBar :: Widget Name
shortcutInfoBar = renderWrappedTxt "[q]: Quit  [j/k]: Up/Down  [n]: New task  [d]: Delete task  [t]: Toggle [o]: Switch list [c]: Calendar view"

currentModeBar :: NadaState -> Widget Name
currentModeBar st = str $ show $ st ^. currentMode

-- Scroll functionality for Todo Viewport
vp0Scroll :: M.ViewportScroll Name
vp0Scroll = M.viewportScroll mkNadaVP

nadaAppDraw :: NadaState -> [Widget Name]
nadaAppDraw st = case _currentMode st of
  ModeCalendar -> [drawCalendar (_calendarState st)]
  _            -> [ui]
  where
    ui = vBox [drawVisibleTodoLists st
              ,currentModeBar st
              ,shortcutInfoBar
              ]

appEventNormal :: BrickEvent Name e -> EventM Name NadaState ()
-- Scroll for Task Viewport
appEventNormal (MouseDown _ E.BScrollDown _ _) = M.vScrollBy vp0Scroll 1
appEventNormal (MouseDown _ E.BScrollUp   _ _) = M.vScrollBy vp0Scroll (-1)
-- Keyboard Shortcuts
appEventNormal (VtyEvent vtyE) = case vtyE of
  V.EvKey (V.KChar 'q') [] -> do
                                _ <- get
                                halt
  V.EvKey (V.KChar 'e') [] -> do
                                st <- get 
                                let selTodoId = getSelectedTodoId st
                                let selTodo = st ^?! todosMap.ix selTodoId
                                todoEditor %= Ed.applyEdit (const $ textZipper (Data.Text.lines $ selTodo ^. todoName) (Just 1))
                                currentMode .= ModeEdit
  V.EvKey (V.KChar 'j') [] -> do
                                listIdx <- use selectedTodoList
                                zoom (visibleTodoLists.ix (fromInteger listIdx)) $
                                  do
                                    tdList <- use todoList
                                    selectedTodo %= min (toInteger $ length tdList - 1) . (+ 1)

  V.EvKey (V.KChar 'k') [] -> do
                                listIdx <- use selectedTodoList
                                zoom (visibleTodoLists.ix (fromInteger listIdx)) $
                                    selectedTodo %= max 0 . subtract 1
  V.EvKey (V.KChar 'd') [] -> do
                                listIdx <- use selectedTodoList
                                zoom (visibleTodoLists.ix (fromInteger listIdx)) $
                                  do
                                    selIdx <- use selectedTodo
                                    todoList %= Seq.deleteAt (fromInteger selIdx)
                                    tdList <- use todoList
                                    selectedTodo %= max 0 . min (toInteger $ length tdList - 1)
  V.EvKey (V.KChar 'n') [] -> do
                                st <- get
                                let (st', newId) = addTodoToState st (defaultTodo 0)
                                put st'
                                listIdx <- use selectedTodoList
                                zoom (visibleTodoLists.ix (fromInteger listIdx)) $
                                  do
                                    todoList %= (Seq.|> newId)
                                    tdList <- use todoList
                                    selectedTodo .= toInteger (length tdList - 1)
  -- Might want to create a lensSelectedTodoList and lensSelectedTodo
  V.EvKey (V.KChar 't') [] -> do
                                st <- get 
                                let selTodoId = getSelectedTodoId st
                                todosMap.ix selTodoId.todoCompleted %= not
  V.EvKey (V.KChar 'o') [] -> do
                                st <- get
                                let nTodoLists = length $ st ^. visibleTodoLists
                                selectedTodoList += 1
                                selectedTodoList %= \i -> if i >= toInteger nTodoLists 
                                                            then 0 
                                                            else i
                                return ()
  V.EvKey (V.KChar 'c') [] -> do
                                freshCalendarState <- liftIO $ makeCalendarStateForCurrentDay
                                todos <- getBareTodosByDate <$> get
                                let cs = freshCalendarState{calendarWidgets = addToCalendarWidgets todos}
                                modify (calendarState .~ cs)
                                modify (currentMode .~ ModeCalendar)
  V.EvKey (V.KChar 'w') [] -> do
                                st <- get
                                let (st', newId) = addTodoToState st (defaultTodo 0)
                                put st'
                                let newList = defaultTodoList & todoListName .~ "More todos"
                                                              & todoList .~ Seq.fromList [newId]
                                visibleTodoLists %= (++ [newList])
                                todoLists <- use visibleTodoLists
                                let nTodoLists = length $ todoLists
                                selectedTodoList .= toInteger nTodoLists - 1
  _ -> return ()
appEventNormal _ = return ()

-- FIXME: This is horrific
getBareTodosByDate :: NadaState -> [(Day, Widget Name)]
getBareTodosByDate st = do
  todo <- Map.elems $ (st ^. todosMap)
  case todo ^. todoDueDate of
    Just dueDate -> pure $ (dueDate, drawTodoSimple todo)
    Nothing -> []
  where
    drawTodoSimple todo = txtWrapWith settings $
      renderCompleted (todo ^. todoCompleted) <> (todo ^. todoName)
    renderCompleted True  = "[X] "
    renderCompleted False = "[ ] "
    settings = defaultWrapSettings { preserveIndentation = True
                                   , breakLongWords = True
                                   }

addToCalendarWidgets :: [(Day, Widget Name)] -> Map Day [Widget Name]
addToCalendarWidgets = foldr (\(day, widget) m -> Map.insertWith (<>) day [widget] m) mempty

appEventEdit :: BrickEvent Name e -> EventM Name NadaState ()
appEventEdit ev = case ev of
  (VtyEvent (V.EvKey V.KEsc [])) -> do
                                      st <- get
                                      let contents = Ed.getEditContents $ st ^. todoEditor
                                      let selTodoId = getSelectedTodoId st
                                      todosMap.ix selTodoId.todoName .= Data.Text.unlines contents
                                      currentMode .= ModeNormal
  _ -> zoom todoEditor $ Ed.handleEditorEvent ev

appEvent :: BrickEvent Name e -> EventM Name NadaState ()
appEvent ev = do
                mode <- use currentMode
                case mode of
                  ModeNormal   -> appEventNormal ev
                  ModeEdit     -> appEventEdit ev
                  ModeCalendar -> appEventCalendar exitCalendar calendarState ev
                  _            -> return ()
  where
    exitCalendar = modify (currentMode .~ ModeNormal)

selectedAttr :: AttrName
selectedAttr = attrName "selected"
editingAttr :: AttrName
editingAttr = attrName "editing"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (selectedAttr, V.black `on` (V.rgbColor 253 253 150))
    , (editingAttr,  V.white `on` V.blue)
    ]

nadaApp :: App NadaState e Name
nadaApp = App
  { appDraw = nadaAppDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = appEvent
  , appStartEvent = do
                      vty <- getVtyHandle
                      liftIO $ V.setMode (V.outputIface vty) V.Mouse True
  , appAttrMap = const theMap
  }
