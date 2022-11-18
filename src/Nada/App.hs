{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada.App
    ( nadaApp
    ) where

import Nada.Types
import Data.Text (pack)


import Brick
import Brick.Main as M
import qualified Brick.Types as T

import Control.Monad.State
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as E


-- Widget - Single Todo
-- [x] task 1
--       description 1
drawTodo :: Todo -> Widget NadaId
drawTodo Todo{..} = 
  padRight (Pad 1) (drawCompleted todoCompleted) 
  <+> txt todoName
  <=> drawDescription
  where
    -- drawCompleted True = clickable todoId $ str ("[X]" ++ (show todoId))
    drawCompleted True = clickable todoId $ txt "[X]"
    drawCompleted False = clickable todoId $ txt "[ ]"
    drawDescription = padLeft (Pad 6) $ txt todoDescription

-- Widget - List of Todos
drawTodos :: NadaState -> Widget NadaId
drawTodos NadaState {..} =  T.Widget T.Fixed T.Fixed $ do
  c <- T.getContext
  let h = T.availHeight c
  T.render 
    $ vLimit (h-2) $ viewport (NadaId 0) Vertical
    $ vBox $ do
               i <- [0..(length todoList - 1)] 
               let mkItem = if toInteger i == selectedTodo
                            then withAttr selectedAttr . visible
                            else id
               return $ mkItem $ drawTodo $ todoList `Seq.index` i

-- Widget - Shortcut info
shortcutInfoBar :: Widget NadaId
shortcutInfoBar = T.Widget T.Fixed T.Fixed $ do
  c <- T.getContext
  let h = T.availHeight c
  T.render 
    $ translateBy (T.Location (0, h-1)) 
    -- $ clickable (NadaId 0)
    $ txt "[q]: Quit  [j/k]: Up/Down  [n]: New task  [d]: Delete task  [t]: Toggle"

-- Scroll functionality for Todo Viewport
vp0Scroll :: M.ViewportScroll NadaId
vp0Scroll = M.viewportScroll (NadaId 0)

nadaAppDraw :: NadaState -> [Widget NadaId]
nadaAppDraw st =
  [ drawTodos st
  , shortcutInfoBar
  ]

appEvent :: BrickEvent NadaId e -> EventM NadaId NadaState ()
-- Modify Tasks
appEvent (MouseDown clickedId E.BLeft _ _) = do
    currentState@NadaState {..} <- get
    let todoIndex = Seq.findIndexL (\Todo{..} -> clickedId == todoId) todoList
    case todoIndex of
      Nothing  -> return ()
      Just i   -> do
                    let newTodoList = Seq.adjust' (\t@Todo{..} -> t{todoCompleted = not todoCompleted}) 
                                              i
                                              todoList
                    put (currentState{todoList = newTodoList})
-- Scroll for Task Viewport
appEvent (MouseDown _ E.BScrollDown _ _) = M.vScrollBy vp0Scroll 1   
appEvent (MouseDown _ E.BScrollUp   _ _) = M.vScrollBy vp0Scroll (-1)
-- Keyboard Shortcuts
appEvent (VtyEvent vtyE) = case vtyE of
  V.EvKey (V.KChar 'q') [] -> do 
                                _ <- get
                                halt
  V.EvKey (V.KChar 'j') [] -> do
                          currentState@NadaState{..} <- get
                          let nextSelected = min (selectedTodo + 1) (toInteger $ length todoList - 1)
                          put $ currentState{selectedTodo = nextSelected}
  V.EvKey (V.KChar 'k') [] -> do
                        currentState@NadaState{..} <- get
                        let nextSelected = max (selectedTodo - 1) 0
                        put $ currentState{selectedTodo = nextSelected}
  -- V.EvKey (V.KChar) [V.Modifiers] -> do sth
  V.EvKey (V.KChar 'd') [] -> do
                                currentState@NadaState{..} <- get
                                let newTodoList = Seq.deleteAt (fromIntegral selectedTodo) todoList
                                let newSelectedTodo = if selectedTodo /= 0 &&
                                                         selectedTodo == toInteger (length todoList - 1)
                                                      then selectedTodo - 1
                                                      else selectedTodo
                                put $ currentState{todoList = newTodoList, selectedTodo = newSelectedTodo}
  V.EvKey (V.KChar 'n') [] -> do 
    currentState@NadaState{..} <- get
    let newId = toInteger (10 + Seq.length todoList)
    let newTodo = Todo
            { todoName = pack ("dummy " ++ show newId)
            , todoDescription = pack ("dummy description " ++ show newId)
            , todoCompleted = False
            , todoId = NadaId newId
            }
    let newSelectedTodo = toInteger $ length todoList
    let newTodoList = Seq.insertAt (Seq.length todoList) newTodo todoList
    put (currentState{todoList = newTodoList, selectedTodo = newSelectedTodo})
  V.EvKey (V.KChar 't') [] -> do
                                currentState@NadaState{..} <- get
                                let currentTodo@Todo{..} = todoList `Seq.index` fromIntegral selectedTodo
                                let newTodoList = Seq.update (fromIntegral selectedTodo)
                                                             currentTodo{todoCompleted = not todoCompleted}
                                                             todoList
                                put (currentState{todoList = newTodoList})
  _ -> return ()
appEvent _ = return ()

selectedAttr :: AttrName
selectedAttr = attrName "selected"

theMap :: AttrMap
theMap = attrMap V.defAttr 
    [ (selectedAttr, V.black `on` V.yellow)
    ]

nadaApp :: App NadaState e NadaId
nadaApp = App
  { appDraw = nadaAppDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = appEvent
  , appStartEvent = do
                      vty <- getVtyHandle
                      liftIO $ V.setMode (V.outputIface vty) V.Mouse True
  , appAttrMap = const theMap
  }
