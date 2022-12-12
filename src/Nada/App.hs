{-# LANGUAGE OverloadedStrings #-}

module Nada.App (
  nadaApp,
) where

import Nada.Types
import Nada.Calendar

import Data.List (intersperse)
import Data.Text (Text, pack, unpack, lines, unlines, words, unwords)
import Data.Text.Zipper (textZipper)
import Data.Time
  ( formatTime
  , defaultTimeLocale
  , showGregorian
  , Day
  )
import Data.Time.Format
import Text.Wrap
  ( defaultWrapSettings
  , preserveIndentation
  , breakLongWords
  , wrapText
  )

import Brick
import qualified Brick.AttrMap as A
import Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Edit as Ed
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as E

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.Platform ()
import Data.Maybe (fromMaybe)

-- FIXME: Separate this function from needing integers and use it with the
-- calendar.
drawTodoBare :: Integer -> Integer -> NadaState -> Widget Name
drawTodoBare iTodo jTodoList st =
  verticalB (thisTodo ^. todoPriority)
  <+>
  (padRight (Pad 1) (drawCompleted (thisTodo ^. todoCompleted))
    <+> drawName
  )
  where
    thisTodoId = st ^?! visibleTodoLists . ix (fromInteger jTodoList) . todoList . ix (fromInteger iTodo)
    thisTodo = st ^?! todosMap . ix thisTodoId
    thisTodoList = st ^?! visibleTodoLists . ix (fromInteger jTodoList)
    listIsSelected = jTodoList == st ^. selectedTodoList
    isSelectedTodo = listIsSelected && iTodo == thisTodoList ^. selectedTodo
    isEditingTodo = isSelectedTodo && (st ^. currentMode == ModeEdit || st ^. currentMode == ModeEditTag || st ^. currentMode == ModeEditDeadline)

    drawName = if isEditingTodo
                  then Ed.renderEditor (txt . Data.Text.unlines) True (st ^. todoEditor)
                  else renderWrappedTxt (thisTodo ^. todoName)
    drawCompleted True  = renderWrappedTxt "[X]"
    drawCompleted False = renderWrappedTxt "[ ]"

-- drawTodo i j st draws the i-th Todo of the j-th TodoList in st
drawTodo :: Integer -> Integer -> NadaState -> Widget Name
drawTodo iTodo jTodoList st =
  withStyle $
    drawTodoBare iTodo jTodoList st
    <=> tags
    <=> padBottom (Pad 1) dueDate
    <=> drawDescription
  where
    thisTodoId     = st ^?! visibleTodoLists.ix (fromInteger jTodoList).todoList.ix (fromInteger iTodo)
    thisTodo       = st ^?! todosMap.ix thisTodoId
    thisTodoList   = st ^?! visibleTodoLists.ix (fromInteger jTodoList)
    listIsSelected = jTodoList == st ^. selectedTodoList
    isSelectedTodo = listIsSelected && iTodo == thisTodoList ^. selectedTodo
    isEditingTodo  = isSelectedTodo && (st ^. currentMode == ModeEdit)
    withStyle
      | isEditingTodo  = forceAttr editingAttr . visible
      | isSelectedTodo = forceAttr selectedAttr . visible
      | otherwise = id

    drawName = if isEditingTodo
               then Ed.renderEditor (txt . Data.Text.unlines) True (st ^. todoEditor)
               else withAttr (attrName "todoname") $ renderWrappedTxt (thisTodo ^. todoName)
    drawCompleted True  = txt "[X]"
    drawCompleted False = txt "[ ]"
    drawDescription = padLeft (Pad 4) $ renderWrappedTxt (thisTodo ^. todoDescription)
    tags = padLeft (Pad 5) $ hBox $ fmap (drawTag st) (thisTodo ^. todoTags)
    dueDate  = padLeft (Pad 5) $ withAttr (attrName "deadline") $ hLimit 20 $ renderWrappedTxt $ pack $
      case thisTodo ^. todoDueDate of
        Nothing -> ""
        Just d  -> "Deadline: " <> formatTime defaultTimeLocale "%Y-%m-%d" d
    drawSubTasks = vBox $ fmap (drawSubTodo st) $ thisTodo ^. todoSubTasks

drawTag :: NadaState -> Text -> Widget Name
drawTag st tag = padRight (Pad 1)$
  updateAttrMap (A.applyAttrMappings (createAttrNames $ st^.allTags)) $
  withAttr (attrName $ unpack tag)
  $ renderWrappedTxt tag

drawSubTodo :: NadaState -> Todo -> Widget Name
drawSubTodo st todo = T.Widget T.Fixed T.Fixed $
  do
    let nextId = st ^. nextAvailableId
        st = st & nextAvailableId .~ nextId + 1
    T.render $
      -- viewport NadaVP nextId Both $
      verticalB (todo ^. todoPriority)
      <+>
      (padRight (Pad 1) (drawCompleted (todo ^. todoCompleted))
        <+> drawName
        <=> tags
        <=> dueDate
        <=> drawDescription
        -- <=> drawSubTasks
      )
      where
        drawName = renderWrappedTxt (todo ^. todoName)
        drawCompleted True  = txt "[X]"
        drawCompleted False = txt "[ ]"
        drawDescription = padLeft (Pad 6) $ renderWrappedTxt (todo ^. todoDescription)
        tags = padLeft (Pad 5) $ (renderWrappedTxt . Data.Text.unwords) (todo ^. todoTags)
        dueDate  = padLeft (Pad 5) $ hLimit 20 $ renderWrappedTxt $ pack $
          case todo ^. todoDueDate of
            Nothing -> ""
            Just d  -> "Deadline: " <> formatTime defaultTimeLocale "%Y-%m-%d" d
        drawSubTasks = vBox $ fmap (drawSubTodo st) $ todo ^. todoSubTasks


renderWrappedTxt :: Text -> Widget Name
renderWrappedTxt t = T.Widget T.Fixed T.Fixed $
  do
    c <- T.getContext
    let w = c ^. T.availWidthL
    T.render $
      (txt . wrapText settings w) t
  where
    settings =
      defaultWrapSettings
        { preserveIndentation = True
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
--     where
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
    High -> [(B.vBorderAttr, red `on` red)]
    Medium -> [(B.vBorderAttr, orange `on` orange)]
    Low -> [(B.vBorderAttr, green `on` green)]
  where
    red = V.rgbColor 255 65 55
    green = V.rgbColor 119 221 119
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
drawTodoList st jTodoList =
  T.Widget T.Greedy T.Greedy $
    T.render $
      ( B.borderWithLabel (txt (tdl ^. todoListName))
        -- Currently no support for scrolling listst
        -- \$ viewport NadaVP Vertical
        $
          vBox $
            intersperse B.hBorder drawnTodos
      )
  where
    tdl = st ^?! visibleTodoLists . ix (fromInteger jTodoList)
    drawnTodos = do
      iTodo <- [0 .. (length (tdl ^. todoList) - 1)]
      return $ drawTodo (toInteger iTodo) jTodoList st

-- case _filterText `isInfixOf` (Data.Text.unlines . Ed.getEditContents . _todoName $ todo) of
--   False -> []
--   True  -> return $ withStyle $ drawTodo todo isFocused

-- Widget - Shortcut info
shortcutInfoBar :: Widget Name
shortcutInfoBar = renderWrappedTxt "[q]: Quit  [j/k]: Up/Down  [n]: New task  [d]: Delete task  [t]: Toggle  [w]: New list  [x]: Delete list  [o]: Switch list  [c]: Calendar view"

-- Widget - Shortcut for edit info
shortcutModifyInfoBar :: Widget Name
shortcutModifyInfoBar = renderWrappedTxt "[w]: Edit deadline  [e]: Edit title  [r]: Edit tags"

currentModeBar :: NadaState -> Widget Name
currentModeBar st = str $ show $ st ^. currentMode

-- Scroll functionality for Todo Viewport
vp0Scroll :: M.ViewportScroll Name
vp0Scroll = M.viewportScroll NadaVP

nadaAppDraw :: NadaState -> [Widget Name]
nadaAppDraw st = case _currentMode st of
  ModeCalendar -> [drawCalendar (Calendar NadaCalendar) (Calendar . NadaCalendarDay) (_calendarState st)]
  _            -> [ui]
  where
    ui =
      vBox
        [ drawVisibleTodoLists st
        , currentModeBar st
        , shortcutModifyInfoBar
        , shortcutInfoBar
        ]

appEventNormal :: BrickEvent Name e -> EventM Name NadaState ()
-- Scroll for Task Viewport
appEventNormal (MouseDown _ E.BScrollDown _ _) = M.vScrollBy vp0Scroll 1
appEventNormal (MouseDown _ E.BScrollUp _ _) = M.vScrollBy vp0Scroll (-1)
-- Keyboard Shortcuts
appEventNormal (VtyEvent vtyE) = case vtyE of
  V.EvKey (V.KChar 'q') [] -> do
    _ <- get
    halt
  V.EvKey (V.KChar 'e') [] -> do
    st <- get
    let selTodoId = getSelectedTodoId st
    let selTodo = st ^?! todosMap . ix selTodoId
    todoEditor %= Ed.applyEdit (const $ textZipper (Data.Text.lines $ selTodo ^. todoName) (Just 1))
    currentMode .= ModeEdit
  V.EvKey (V.KChar 'r') [] -> do
    st <- get
    let selTodoId = getSelectedTodoId st
    let selTodo = st ^?! todosMap . ix selTodoId
    todoEditor %= Ed.applyEdit (const $ textZipper [Data.Text.unwords (selTodo ^. todoTags)] (Just 1))
    currentMode .= ModeEditTag
  V.EvKey (V.KChar 'w') [] -> do
    st <- get
    let selTodoId = getSelectedTodoId st
    let selTodo = st ^?! todosMap . ix selTodoId
    todoEditor %= Ed.applyEdit (const $ textZipper (showDate (selTodo ^. todoDueDate)) (Just 1))
    currentMode .= ModeEditDeadline
  V.EvKey (V.KChar 'j') [] -> do
    listIdx <- use selectedTodoList
    zoom (visibleTodoLists . ix (fromInteger listIdx)) $
      do
        tdList <- use todoList
        selectedTodo %= min (toInteger $ length tdList - 1) . (+ 1)
  V.EvKey (V.KChar 'k') [] -> do
    listIdx <- use selectedTodoList
    zoom (visibleTodoLists . ix (fromInteger listIdx)) $
      selectedTodo %= max 0 . subtract 1
  V.EvKey (V.KChar 'd') [] -> do
    listIdx <- use selectedTodoList
    zoom (visibleTodoLists . ix (fromInteger listIdx)) $
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
    zoom (visibleTodoLists . ix (fromInteger listIdx)) $
      do
        todoList %= (Seq.|> newId)
        tdList <- use todoList
        selectedTodo .= toInteger (length tdList - 1)
  -- Might want to create a lensSelectedTodoList and lensSelectedTodo
  V.EvKey (V.KChar 't') [] -> do
    st <- get
    let selTodoId = getSelectedTodoId st
    todosMap . ix selTodoId . todoCompleted %= not
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
                                todos <- gets getBareTodosByDate
                                let cs = freshCalendarState{calendarWidgets = makeCalendarWidgets todos}
                                modify (calendarState .~ cs)
                                modify (currentMode .~ ModeCalendar)
  V.EvKey (V.KChar 'w') [] -> do
                                st <- get
                                let (st', newId) = addTodoToState st (defaultTodo 0)
                                put st'
                                let newList = defaultTodoList & todoListName .~ "More todos"
                                                              & todoList .~ Seq.fromList [newId]
                                id %= addTodoListToState newList
                                todoLists <- use visibleTodoLists
                                let nTodoLists = length todoLists
                                selectedTodoList .= toInteger nTodoLists - 1
  V.EvKey (V.KChar 'x') [] -> do
                                todoLists <- use visibleTodoLists
                                sel <- use selectedTodoList
                                let (l, r) = splitAt (fromInteger sel) todoLists
                                let newTodoLists = l ++ tail r
                                visibleTodoLists .= newTodoLists
                                selectedTodoList %= max 0 . min (toInteger $ length newTodoLists - 1)

  _ -> return ()
appEventNormal _ = return ()

-- FIXME: This is horrific
getBareTodosByDate :: NadaState -> [(Day, Widget Name)]
getBareTodosByDate st = do
  todo <- Map.elems (st ^. todosMap)
  case todo ^. todoDueDate of
    Just dueDate -> pure (dueDate, drawTodoSimple todo)
    Nothing -> []
  where
    drawTodoSimple todo = txtWrapWith settings $
      renderCompleted (todo ^. todoCompleted) <> (todo ^. todoName)
    renderCompleted True  = "[X] "
    renderCompleted False = "[ ] "
    settings = defaultWrapSettings { preserveIndentation = True
                                   , breakLongWords = True
                                   }

makeCalendarWidgets :: [(Day, Widget Name)] -> (Day -> [Widget Name])
makeCalendarWidgets daysToWidgets day = fromMaybe [] (Map.lookup day dayToWidgetMap)
  where
    dayToWidgetMap = foldr (\(d, widget) m -> Map.insertWith (<>) d [widget] m) mempty daysToWidgets

appEventEdit :: BrickEvent Name e -> EventM Name NadaState ()
appEventEdit ev = case ev of
  (VtyEvent (V.EvKey V.KEsc [])) -> do
    st <- get
    let contents = Ed.getEditContents $ st ^. todoEditor
    let selTodoId = getSelectedTodoId st
    let theText = Data.Text.unlines contents
    mode <- use currentMode
    case mode of
      ModeEdit -> todosMap . ix selTodoId . todoName .= theText
      ModeEditTag -> todosMap . ix selTodoId . todoTags .= Data.Text.words theText
      ModeEditDeadline ->
        todosMap . ix selTodoId . todoDueDate .=
          if theText /= "\n"
            then parseTimeM True defaultTimeLocale "%Y-%m-%d" (Data.Text.unpack theText) :: Maybe Day
            else Nothing
    currentMode .= ModeNormal
  _ -> zoom todoEditor $ Ed.handleEditorEvent ev

appEvent :: BrickEvent Name e -> EventM Name NadaState ()
appEvent ev = do
                mode <- use currentMode
                case mode of
                  ModeNormal       -> appEventNormal ev
                  ModeEdit         -> appEventEdit ev
                  ModeEditTag      -> appEventEdit ev
                  ModeEditDeadline -> appEventEdit ev
                  ModeCalendar     -> appEventCalendarLens calendarState nameToDay exitCalendar ev
  where
    exitCalendar = modify (currentMode .~ ModeNormal)

selectedAttr :: AttrName
selectedAttr = attrName "selected"
editingAttr :: AttrName
editingAttr = attrName "editing"

createAttrNames :: [Text] -> [(AttrName, V.Attr)]
createAttrNames tl = Prelude.zip names myAttrs
  where
    names = fmap (attrName . unpack) tl

myAttrs :: [V.Attr]
myAttrs = [ fg V.yellow
          , fg V.magenta
          , fg V.brightCyan
          , fg V.brightRed
          , fg V.brightGreen
          , fg V.brightBlue
          , fg V.brightYellow
          , fg V.cyan
          , fg V.red
          --   V.white  `on` V.green
          -- , V.yellow `on` V.black
          -- , V.white  `on` V.red
          -- , V.white  `on` V.magenta
          -- , V.white  `on` V.cyan
          ]

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (selectedAttr, V.black `on` V.rgbColor 253 253 150)
    , (editingAttr, V.white `on` V.blue)
    , (attrName "deadline",  V.withStyle (fg V.blue) V.italic)
    , (attrName "todoname", V.withStyle V.defAttr V.bold)
    -- , (Ed.editAttr,                   V.white `on` V.blue)
    -- , (Ed.editFocusedAttr,            V.black `on` V.yellow)
    ]

nadaApp :: App NadaState e Name
nadaApp =
  App
    { appDraw = nadaAppDraw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = appEvent
    , appStartEvent = do
        vty <- getVtyHandle
        liftIO $ V.setMode (V.outputIface vty) V.Mouse True
    , appAttrMap = const theMap
    }
