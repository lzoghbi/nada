{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Nada.App
    ( nadaApp
    ) where

import Nada.Types

import qualified Data.Sequence as Seq
import Data.List (intersperse)
import Data.Text (Text, unlines, unwords, lines)
import Data.Text.Zipper (textZipper)
import Data.Time
  ( formatTime
  , defaultTimeLocale
  )
import Text.Wrap
  (defaultWrapSettings
  , preserveIndentation
  , breakLongWords
  , wrapText
  , wrapTextToLines
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
import qualified Data.Text.Zipper as Z hiding ( textZipper )
import qualified Data.Text.Zipper.Generic as Z
import qualified Data.Text.Zipper.Generic.Words as Z

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.Platform()

-- drawTodo i j st draws the i-th Todo of the j-th TodoList in st
drawTodo :: Integer -> Integer -> NadaState -> Widget Name
drawTodo iTodo jTodoList st =
  withStyle $
  verticalB (thisTodo ^. todoPriority)
  <+>
  (padRight (Pad 1) (drawCompleted (thisTodo ^. todoCompleted))
    -- below the context of the editor is rendered, but the cursor is not visible yet - to be fixed soon
    -- if you want to see the cursor, but with no text rendering, uncomment the following
    -- <+> renderWrappedTxt ((Data.Text.unlines (Ed.getEditContents _todoName)))  
    <+> drawName
    <=> tags
    <=> dueDate
    <=> drawDescription
  )
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

    drawName = if isEditingTodo
                  then Ed.renderEditor (txt . Data.Text.unlines) True (st ^. todoEditor)
                  else renderWrappedTxt (thisTodo ^. todoName)
    drawCompleted True  = txt "[X]"
    drawCompleted False = txt "[ ]"
    drawDescription = padLeft (Pad 6) $ renderWrappedTxt (thisTodo ^. todoDescription)
    tags = padLeft (Pad 4) $ (renderWrappedTxt . Data.Text.unwords) (thisTodo ^. todoTags)
    dueDate  = padLeft (Pad 4) $ hLimit 20 $ strWrapWith settings $
      case thisTodo ^. todoDueDate of
        Nothing -> ""
        Just d  -> "Deadline: " <> formatTime defaultTimeLocale "%Y-%d-%m" d
      where
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


-- renderEd :: (Ord n, Show n, Monoid t, TextWidth t, Z.GenericTextZipper t)
--              => Bool -> Ed.Editor t n -> Widget n

-- renderEd foc e = T.Widget T.Fixed T.Fixed 
--   $ do
--     c <- T.getContext 
--     let w  = c^.T.availWidthL
--         cp = Ed.getCursorPosition e
--         z  = e^.Ed.editContentsL
--         line = Z.currentLine z
--         tw   = textWidth line
--         lim = if (tw) > w  
--               then tw `div` w + 1
--               else 1  
--         toLeft = Z.take (cp^._2) line
--         cursorLoc = Location (textWidth toLeft, cp^._1)
--         atChar = charAtCursor 10  (e^.Ed.editContentsL)
--         atCharWidth = maybe 1 textWidth atChar
--     T.render $ 
--      vLimit lim $
--      viewport (getName e) Both $
--      withAttr (if foc then Ed.editFocusedAttr else Ed.editAttr) $      
--      (if foc then showCursor (getName e) cursorLoc else id) $
--      visibleRegion cursorLoc (atCharWidth, 1) $
--      txt $ 
--      "Running nada without any command should be thedd same as running nadaggg with the command editnnnnnnn" 
--     --  f (Data.Text.unlines (getText (Ed.getEditContents e))) 
     

--       -- (txt . wrapText settings w ) (Data.Text.unlines $ Z.getText z)
--       where
--         -- f = txt . wrapText settings w 
--         settings = defaultWrapSettings { preserveIndentation = True
--                                        , breakLongWords = True
--                                        }


-- charAtCursor :: (Z.GenericTextZipper t) => Int -> Z.TextZipper t -> Maybe t
-- charAtCursor maxW z =
--     let col = snd $ Z.cursorPosition z
--         curLine = Z.currentLine z
--         toRight = Z.drop col curLine
--         length  = Z.length toRight
--     in if length > 0 
--        then if length <= 5 
--             then Just $ Z.take 1 toRight
--             -- else curLine+1
--             else Just $ Z.take 1 toRight
--        else Nothing

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
      i <- [0..(length (st ^. visibleTodoLists) - 1)]
      return $ drawTodoList st (toInteger i)

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
shortcutInfoBar = renderWrappedTxt "[q]: Quit  [j/k]: Up/Down  [n]: New task  [d]: Delete task  [t]: Toggle [o]: Switch list"

currentModeBar :: NadaState -> Widget Name
currentModeBar st = str $ show $ st ^. currentMode

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
  _ -> return ()
appEventNormal _ = return ()

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
                if mode == ModeNormal
                  then appEventNormal ev
                else if mode == ModeEdit
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
    -- , (attrName "tag1", V.white `on` V.green)
    -- , (attrName "tag2", V.white `on` V.green)
    -- , (attrName "tag3", V.white `on` V.green)
    -- , (attrName "tag4", V.white `on` V.green)
    -- , (attrName "tag5", V.white `on` V.green)
    -- , (attrName "tag6", V.white `on` V.green)
    -- , (attrName "tag7", V.white `on` V.green)
    -- , (attrName "tag8", V.white `on` V.green)
    -- , (attrName "tag9", V.white `on` V.green)
    -- , (attrName "tag10", V.white `on` V.green)
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
