{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada.App
    ( nadaApp
    ) where

import Nada.Types

import qualified Data.Sequence as Seq
import Data.List (intersperse)
import Data.Text (Text, unlines, unwords)
import Data.Time
  ( formatTime
  , defaultTimeLocale
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
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as E

import Control.Monad.State

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.Platform()

-- Widget - Single Todo
-- [x] task 1
--       description 1
drawTodo :: Todo -> Bool -> Widget Name
drawTodo Todo{..} b =
  verticalB _todoPriority
  <+>
  (padRight (Pad 1) (drawCompleted _todoCompleted)
    -- below the context of the editor is rendered, but the cursor is not visible yet - to be fixed soon
    -- if you want to see the cursor, but with no text rendering, uncomment the following
    <+> Ed.renderEditor (txt . Data.Text.unlines) b _todoName
    -- <+> renderWrappedTxt ((Data.Text.unlines (Ed.getEditContents _todoName)))  
    <=> tags
    <=> dueDate
    <=> drawDescription
  )
  where
    drawCompleted True  = clickable _todoId $ txt "[X]"
    drawCompleted False = clickable _todoId $ txt "[ ]"
    drawDescription = padLeft (Pad 6) $ renderWrappedTxt _todoDescription
    tags = padLeft (Pad 4) $ (renderWrappedTxt . Data.Text.unwords) _todoTags
    dueDate  = padLeft (Pad 4) $ hLimit 20 $ strWrapWith settings $
      case _todoDueDate of
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
      i <- [0..(length (st^.visibleTodoLists) - 1)]
      return $ drawTodoList st (toInteger i)

-- Draw the TodoList at the specified index
drawTodoList :: NadaState -> Integer -> Widget Name
drawTodoList st todoListIdx = T.Widget T.Greedy T.Greedy $
  T.render $
    ( B.borderWithLabel (txt (tdl^.todoListName))
    $ viewport NadaVP Vertical
    $ vBox
    $ intersperse B.hBorder drawnTodos
    )
  where
    tdl = st^?!visibleTodoLists.ix (fromInteger todoListIdx)
    listIsSelected = todoListIdx == st^.selectedTodoList
    drawnTodos = do
        i <- [0..(length (tdl^.todoList) - 1)]
        let isSelected = listIsSelected && toInteger i == tdl^.selectedTodo
        let isFocused = isSelected && (st^.currentMode == Edit)
        let withStyle
              | isFocused = forceAttr editingAttr . visible
              | isSelected = forceAttr selectedAttr . visible
              | otherwise = id
        let tdId = (tdl^.todoList) `Seq.index` i
        let todo = st^?!todosMap.ix tdId
        return $ withStyle $ drawTodo todo isFocused
  -- case _filterText `isInfixOf` (Data.Text.unlines . Ed.getEditContents . _todoName $ todo) of
  --   False -> []
  --   True  -> return $ withStyle $ drawTodo todo isFocused

-- Widget - Shortcut info
shortcutInfoBar :: Widget Name
shortcutInfoBar = renderWrappedTxt "[q]: Quit  [j/k]: Up/Down  [n]: New task  [d]: Delete task  [t]: Toggle"

currentModeBar :: NadaState -> Widget Name
currentModeBar st = str $ show $ st^.currentMode

-- Scroll functionality for Todo Viewport
vp0Scroll :: M.ViewportScroll Name
vp0Scroll = M.viewportScroll NadaVP

nadaAppDraw :: NadaState -> [Widget Name]
nadaAppDraw st = [ui]
  where
    ui = vBox [drawVisibleTodoLists st
              ,currentModeBar st
              ,shortcutInfoBar
              ]

appEventNormal :: BrickEvent Name e -> EventM Name NadaState ()
-- Modify Tasks
-- appEventNormal (MouseDown clickedId E.BLeft _ _) = do
--     currentState@NadaState {..} <- get
--     let todoIndex = Seq.findIndexL (\Todo{..} -> clickedId == _todoId) _todoList
--     case todoIndex of
--       Nothing  -> return ()
--       Just i   -> do
--                     let newTodoList = Seq.adjust' (\t@Todo{..} -> t{_todoCompleted = not _todoCompleted}) 
--                                               i
--                                               _todoList
--                     put (currentState{_todoList = newTodoList})
-- Scroll for Task Viewport
appEventNormal (MouseDown _ E.BScrollDown _ _) = M.vScrollBy vp0Scroll 1
appEventNormal (MouseDown _ E.BScrollUp   _ _) = M.vScrollBy vp0Scroll (-1)
-- Keyboard Shortcuts
appEventNormal (VtyEvent vtyE) = case vtyE of
  V.EvKey (V.KChar 'q') [] -> do
                                _ <- get
                                halt
  V.EvKey (V.KChar 'e') [] -> currentMode .= Edit
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
                                let listIdx = st^.selectedTodoList
                                let currentTodoList = visibleTodoLists.ix (fromInteger listIdx)
                                let selIdx = st^?!currentTodoList.selectedTodo
                                let selTodoId = st^?!(currentTodoList.todoList.ix (fromInteger selIdx))
                                todosMap.ix selTodoId.todoCompleted %= not
  _ -> return ()
appEventNormal _ = return ()

appEventEdit :: BrickEvent Name e -> EventM Name NadaState ()
appEventEdit ev = case ev of
  (VtyEvent (V.EvKey V.KEsc [])) -> currentMode .= Normal
  _ -> do
          st <- get 
          let listIdx = st^.selectedTodoList
          let currentTodoList = visibleTodoLists.ix (fromInteger listIdx)
          let selIdx = st^?!currentTodoList.selectedTodo
          let selTodoId = st^?!(currentTodoList.todoList.ix (fromInteger selIdx))
          zoom (todosMap.ix selTodoId.todoName) $ Ed.handleEditorEvent ev

appEvent :: BrickEvent Name e -> EventM Name NadaState ()
appEvent ev = do
                mode <- use currentMode
                if mode == Normal
                  then appEventNormal ev
                else if mode == Edit
                  then appEventEdit ev
                else return ()

selectedAttr :: AttrName
selectedAttr = attrName "selected"
editingAttr :: AttrName
editingAttr = attrName "editing"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (selectedAttr, V.black `on` (V.rgbColor 253 253 150))
    , (editingAttr,  V.white `on` V.blue)
    -- , (Ed.editAttr,                   V.white `on` V.blue)
    -- , (Ed.editFocusedAttr,            V.black `on` V.yellow)
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
