{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada.App
    ( nadaApp
    ) where

import Nada.Types
import Data.Text (pack, unlines)


import Brick
import Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as Ed

import Control.Monad.State
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as E

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.Platform()


-- Widget - Single Todo
-- [x] task 1
--       description 1
drawTodo :: Todo -> Bool -> Widget Name
drawTodo Todo{..} b = 
  padRight (Pad 1) (drawCompleted _todoCompleted) 
  <+> Ed.renderEditor (txt . Data.Text.unlines) b _todoName
  <=> drawDescription
  where
    drawCompleted True = clickable _todoId $ txt "[X]"
    drawCompleted False = clickable _todoId $ txt "[ ]"
    drawDescription = padLeft (Pad 6) $ txt _todoDescription

-- Widget - List of Todos
drawTodos :: NadaState -> Widget Name
drawTodos NadaState {..} =  T.Widget T.Greedy T.Greedy $ do
  T.render 
    $ viewport NadaVP Vertical
    $ vBox $ do
              i <- [0..(length _todoList - 1)] 
              let isSelected = toInteger i == _selectedTodo
              let isFocused = isSelected && (_mode == Edit)
              let withStyle 
                    | isFocused = forceAttr editingAttr . visible
                    | isSelected = forceAttr selectedAttr . visible
                    | otherwise = id
              return $ withStyle $ drawTodo (_todoList `Seq.index` i) isFocused

-- Widget - Shortcut info
shortcutInfoBar :: Widget Name
shortcutInfoBar = txt "[q]: Quit  [j/k]: Up/Down  [n]: New task  [d]: Delete task  [t]: Toggle"

currentModeBar :: NadaState -> Widget Name
currentModeBar NadaState{..} = str $ show _mode

-- Scroll functionality for Todo Viewport
vp0Scroll :: M.ViewportScroll Name
vp0Scroll = M.viewportScroll NadaVP

nadaAppDraw :: NadaState -> [Widget Name]
nadaAppDraw st = [ui]
  where
    ui = vBox [drawTodos st 
              ,currentModeBar st
              ,shortcutInfoBar
              ]

appEventNormal :: BrickEvent Name e -> EventM Name NadaState ()
-- Modify Tasks
appEventNormal (MouseDown clickedId E.BLeft _ _) = do
    currentState@NadaState {..} <- get
    let todoIndex = Seq.findIndexL (\Todo{..} -> clickedId == _todoId) _todoList
    case todoIndex of
      Nothing  -> return ()
      Just i   -> do
                    let newTodoList = Seq.adjust' (\t@Todo{..} -> t{_todoCompleted = not _todoCompleted}) 
                                              i
                                              _todoList
                    put (currentState{_todoList = newTodoList})
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
                                put $ st {_mode = Edit}
  V.EvKey (V.KChar 'j') [] -> do
                          currentState@NadaState{..} <- get
                          let nextSelected = min (_selectedTodo + 1) (toInteger $ length _todoList - 1)
                          put $ currentState{_selectedTodo = nextSelected}
  V.EvKey (V.KChar 'k') [] -> do
                        currentState@NadaState{..} <- get
                        let nextSelected = max (_selectedTodo - 1) 0
                        put $ currentState{_selectedTodo = nextSelected}
  V.EvKey (V.KChar 'd') [] -> do
                                currentState@NadaState{..} <- get
                                let newTodoList = Seq.deleteAt (fromIntegral _selectedTodo) _todoList
                                let newSelectedTodo = if _selectedTodo /= 0 &&
                                                         _selectedTodo == toInteger (length _todoList - 1)
                                                      then _selectedTodo - 1
                                                      else _selectedTodo
                                put $ currentState{_todoList = newTodoList, _selectedTodo = newSelectedTodo}
  V.EvKey (V.KChar 'n') [] -> do 
    currentState@NadaState{..} <- get
    let newId = toInteger (10 + Seq.length _todoList)
    let newTodo = Todo
            { _todoName = Ed.editorText (EditorId newId) (Just 1) (pack $ "dummy " ++ show newId)
            , _todoDescription = pack ("dummy description " ++ show newId)
            , _todoCompleted = False
            , _todoId = TodoId newId
            }
    let newSelectedTodo = toInteger $ length _todoList
    let newTodoList = Seq.insertAt (Seq.length _todoList) newTodo _todoList
    put (currentState{_todoList = newTodoList, _selectedTodo = newSelectedTodo})
  V.EvKey (V.KChar 't') [] -> do
                                currentState@NadaState{..} <- get
                                let currentTodo@Todo{..} = _todoList `Seq.index` fromIntegral _selectedTodo
                                let newTodoList = Seq.update (fromIntegral _selectedTodo)
                                                             currentTodo{_todoCompleted = not _todoCompleted}
                                                             _todoList
                                put (currentState{_todoList = newTodoList})
  _ -> return ()
appEventNormal _ = return ()

appEventEdit :: BrickEvent Name e -> EventM Name NadaState ()
appEventEdit ev = case ev of
  (VtyEvent (V.EvKey V.KEsc [])) -> do
    st <- get
    put $ st {_mode = Normal}
  _ -> do
    idx <- use selectedTodo 
    zoom (todoList.ix (fromIntegral idx).todoName) $ Ed.handleEditorEvent ev

appEvent :: BrickEvent Name e -> EventM Name NadaState ()
appEvent ev = do
                NadaState {..} <- get
                if _mode == Normal
                  then appEventNormal ev
                else if _mode == Edit
                  then appEventEdit ev
                else return ()

-- selectedAttr :: AttrName
selectedAttr :: AttrName
selectedAttr = attrName "selected"
editingAttr :: AttrName
editingAttr = attrName "editing"

theMap :: AttrMap
theMap = attrMap V.defAttr 
    [ (selectedAttr,                  V.black `on` V.yellow)
    , (editingAttr,                   V.white `on` V.blue)
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
