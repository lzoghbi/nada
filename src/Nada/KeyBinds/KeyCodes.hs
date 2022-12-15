module Nada.KeyBinds.KeyCodes
  (
    showUserInputEvent
  , parseKeyCode
  ) where

import Data.Char (toLower)
import Data.List (sort)
import Text.Read (readMaybe)
import Control.Monad (guard)
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Graphics.Vty.Input.Events

-- | Displays the keycode notation for the user input event.
-- Notation is somewhat arbitrary... sorry.
-- Returns 'Nothing' if the event isn't a user input event
-- (e.g. 'EvResize').
showUserInputEvent :: Event -> Maybe String
showUserInputEvent event = case event of
  EvKey key modifiers -> Just $ showModifiers modifiers <> showKey key
  -- If you're using Brick, it's unlikely that you will be using either of these,
  -- It might even be worth just sending them to 'Nothing' directly.
  EvMouseDown _ _ mouseButton modifiers -> Just $ showModifiers modifiers <> showMouseButton mouseButton
  EvMouseUp _ _ mouseButtonMaybe -> Just $ maybe "Mouse" showMouseButton mouseButtonMaybe <> "Released"
  _ -> Nothing

showModifiers :: [Modifier] -> String
showModifiers ms = show
  -- Reverse sorting happens to put them in alpha order, which is at least
  -- what Emacs does.
  (reverse . sort $ ms)
  where
    show = concatMap (appendDash . showModifier)
    appendDash = (<> "-")

showModifier :: Modifier -> String
showModifier modifier = case modifier of
  MShift -> "S"
  MCtrl  -> "C"
  MMeta  -> "M"
  MAlt   -> "A"

showKey :: Key -> String
showKey key = case key of
  KEsc -> "Esc"
  KChar ' '  -> "Space"
  KChar '\t' -> "Tab"
  KChar c -> [c]
  KBS -> "Backspace"
  KEnter -> "Enter"
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
  KPageUp -> "PageUp"
  KDel -> "Del"
  KEnd -> "End"
  KPageDown -> "PageDown"
  KBegin -> "Begin"
  KMenu-> "Menu"

showMouseButton :: Button -> String
showMouseButton mouseButton = case mouseButton of
  BLeft -> "LeftClick"
  BMiddle -> "MiddleClick"
  BRight -> "RightClick"
  BScrollUp -> "MouseWheelUp"
  BScrollDown -> "MouseWheelDown"

-- | Matches a special key (pretty much whatever isn't 'KChar' or 'KFun').
-- This is very lenient where reasonable (e.g. ignoring case, permitting
-- "SPC" or "Space") because I see no reason to be strict about how you
-- refer to keycodes. "Where reasonable" means that we won't parse for
-- example "esca" since that isn't used.
parseSpecialKeyCode :: String -> Maybe Key
parseSpecialKeyCode s = case s of
  -- Escape
  "esc" -> Just KEsc
  "escape" -> Just KEsc
  -- Space
  "spc" -> Just $ KChar ' '
  "space" -> Just $ KChar ' '
  "tab" -> Just $ KChar '\t'
  -- Backspace
  "bs" -> Just KBS
  "back" -> Just KBS
  "backspace" -> Just KBS
  -- Return
  "ent" -> Just KEnter
  "enter" -> Just KEnter
  "ret" -> Just KEnter
  "return" -> Just KEnter
  "left" -> Just KLeft
  "right" -> Just KRight
  "up" -> Just KUp
  "down" -> Just KDown
  -- UpLeft
  "upleft" -> Just KUpLeft
  "leftup" -> Just KUpLeft
  -- UpRight
  "upright" -> Just KUpRight
  "rightup" -> Just KUpRight
  -- DownLeft
  "downleft" -> Just KDownLeft
  "leftdown" -> Just KDownLeft
  -- DownRight
  "downright" -> Just KDownRight
  "rightdown" -> Just KDownRight
  "center" -> Just KCenter
  "backtab" -> Just KBackTab
  -- Print Screen
  "prtscr" -> Just KPrtScr
  "printscreen" -> Just KPrtScr
  "pause" -> Just KPause
  -- Insert
  "insert" -> Just KIns
  "ins" -> Just KIns
  "home" -> Just KHome
  -- Page Up
  "pgup" -> Just KPageUp
  "pageup" -> Just KPageUp
  -- Delete
  "del" -> Just KDel
  "delete" -> Just KDel
  "end" -> Just KEnd
  -- Page Down
  "pgdwn" -> Just KPageDown
  "pgdown" -> Just KPageDown
  "pagedown" -> Just KPageDown
  "begin" -> Just KBegin
  "menu" -> Just KMenu
  _ -> Nothing

parseFunctionKeyCode :: String -> Maybe Key
parseFunctionKeyCode ('f':numStr) = do
  num <- readMaybe numStr
  -- According to Graphics.Vty.Input.Events this is what
  -- KFun can range from. No clue where F63 is on my
  -- computer.
  guard (num > 0 && num < 64)
  pure $ KFun num
parseFunctionKeyCode _ = Nothing

-- | This is not a sophisticated function, so if something
-- suspicious leaks through, e.g. a literal space character,
-- it will successfully parse.
parseCharKeyCode :: String -> Maybe Key
parseCharKeyCode [c] = Just $ KChar c
parseCharKeyCode _ = Nothing

parseModifiers :: String -> ([Modifier], String)
parseModifiers s = case s of
  ('a':'-':rest) -> first (MAlt:) $ parseModifiers rest
  ('c':'-':rest) -> first (MCtrl:) $ parseModifiers rest
  ('m':'-':rest) -> first (MMeta:) $ parseModifiers rest
  ('s':'-':rest) -> first (MShift:) $ parseModifiers rest
  rest -> ([], rest)

parseKeyCode :: String -> Maybe Event
parseKeyCode str = (\k -> EvKey k modifiers) <$> keyCode
  where
    lowered = map toLower str
    (modifiers, kc) = parseModifiers lowered
    keyCode = parseSpecialKeyCode kc <|> parseFunctionKeyCode kc <|> parseCharKeyCode kc
