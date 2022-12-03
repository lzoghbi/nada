{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada.App
    ( nadaApp
    ) where

import Nada.Types
import Data.List (intersperse)
import Data.Text (Text, pack, unlines, unwords, isInfixOf)
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
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS

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
  (verticalB _todoPriority) 
  <+>
  (padRight (Pad 1) (drawCompleted _todoCompleted) 
    -- below the context of the editor is rendered, but the cursor is not visible yet - to be fixed soon
    -- if you want to see the cursor, but with no text rendering, uncomment the following
    -- Ed.renderEditor (renderWrappedTxt . Data.Text.unlines) b _todoName
    <+> renderWrappedTxt ((Data.Text.unlines (Ed.getEditContents _todoName)))  
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
    let h = c^.T.availHeightL 
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

-- Widget - List of Todos
drawTodos :: NadaState -> Widget Name
drawTodos st =  T.Widget T.Greedy T.Greedy $ 
  T.render $
    ( B.borderWithLabel (str "All tasks") 
    $ viewport NadaVP Vertical
    $ vBox 
    $ intersperse B.hBorder
    $ getAllTodos st
    )
    -- Make other categories of tasks, like overdue tasks or filtered by tag
    <+>
    ( B.borderWithLabel (str "Other tasks") 
    $ vBox 
    $ intersperse B.hBorder
    $ getAllTodos testNadaState
    )

getAllTodos :: NadaState -> [Widget Name]
getAllTodos NadaState {..} = do
  i <- [0..(length _todoList - 1)] 
  let isSelected = toInteger i == _selectedTodo
  let isFocused = isSelected && (_mode == Edit)
  let withStyle 
        | isFocused = forceAttr editingAttr . visible
        | isSelected = forceAttr selectedAttr . visible
        | otherwise = id
  let todo = _todoList `Seq.index` i
  case _filterText `isInfixOf` (Data.Text.unlines . Ed.getEditContents . _todoName $ todo) of
    False -> []
    True  -> return $ withStyle $ drawTodo todo isFocused

-- Widget - Shortcut info
shortcutInfoBar :: Widget Name
shortcutInfoBar = renderWrappedTxt "[q]: Quit  [j/k]: Up/Down  [n]: New task  [d]: Delete task  [t]: Toggle"

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
            , _todoDueDate  = Nothing
            , _todoPriority = Medium
            , _todoTags = []
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
