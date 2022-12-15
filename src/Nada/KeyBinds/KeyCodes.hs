module Nada.KeyBinds.KeyCodes
  (
    renderUserInputEvent
  ) where

import Data.List (sort)
import Graphics.Vty

-- | Displays the keycode notation for the user input event.
-- Notation is somewhat arbitrary... sorry.
-- Returns 'Nothing' if the event isn't a user input event
-- (e.g. 'EvResize').
renderUserInputEvent :: Event -> Maybe String
renderUserInputEvent event = case event of
  EvKey key modifiers -> Just $ renderModifiers modifiers <> renderKey key
  -- If you're using Brick, it's unlikely that you will be using either of these,
  -- It might even be worth just sending them to 'Nothing' directly.
  EvMouseDown _ _ mouseButton modifiers -> Just $ renderModifiers modifiers <> renderMouseButton mouseButton
  EvMouseUp _ _ mouseButtonMaybe -> Just $ maybe "Mouse" renderMouseButton mouseButtonMaybe <> "Released"
  _ -> Nothing

renderModifiers :: [Modifier] -> String
renderModifiers ms = render (reverse . sort $ ms)
  where
    render = concatMap (appendDash . renderModifier)
    appendDash = (<> "-")

renderModifier :: Modifier -> String
renderModifier modifier = case modifier of
  MShift -> "S"
  MCtrl  -> "C"
  MMeta  -> "M"
  MAlt   -> "A"

renderKey :: Key -> String
renderKey key = case key of
  KEsc -> "Esc"
  KChar ' '  -> "Space"
  KChar '\t' -> "Tab"
  KChar c -> [c]
  KBS -> "Back"
  KEnter -> "Ret"
  KLeft -> "Left"
  KRight -> "Right"
  KUp -> "Up"
  KDown -> "Down"
  KUpLeft -> "UpLeft"
  KUpRight -> "UpRight"
  KDownLeft -> "DownLeft"
  KDownRight -> "DownRight"
  KCenter -> "Center"
  KFun i -> "F" <> show i
  KBackTab -> "BackTab"
  KPrtScr -> "PrtScr"
  KPause -> "Pause"
  KIns -> "Insert"
  KHome -> "Home"
  KPageUp -> "PgUp"
  KDel -> "Del"
  KEnd -> "End"
  KPageDown -> "PgDown"
  KBegin -> "Begin"
  KMenu-> "Menu"

renderMouseButton :: Button -> String
renderMouseButton mouseButton = case mouseButton of
  BLeft -> "LeftClick"
  BMiddle -> "MiddleClick"
  BRight -> "RightClick"
  BScrollUp -> "MouseWheelUp"
  BScrollDown -> "MouseWheelDown"
