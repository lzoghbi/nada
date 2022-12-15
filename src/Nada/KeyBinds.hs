{-# LANGUAGE RecordWildCards #-}
module Nada.KeyBinds
  ( -- * 'KeyBind'
    KeyBind
  , keyBind
    -- * 'KeyBinds'
  , KeyBindings
  , keybindings
  , drawHelpMenu
  , appEventKeyBinds
  ) where

import Nada.KeyBinds.KeyCodes

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (centerLayer)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)

data KeyBind n s = KeyBind
  { kbEvent :: EventM n s ()
  , kbName :: Maybe Text
  , kbDesc :: Maybe Text
  , kbKeys :: [KeyCode] -- ^ You should not have to set this yourself.
                           -- It should be set automatically 'keybinds'.
  , kbShowHelp :: Bool
  }

-- | Smart constructor that creates a 'KeyBind'.
-- If you don't want to provide a name and description, see 'fromEvent'.
--
-- Initializes the 'kbKeys' to the empty list - it will be autopopulated in 'keybinds'.
keyBind :: EventM n s () -> Maybe Text -> Maybe Text -> Bool -> KeyBind n s
keyBind handle name desc showHelp = KeyBind handle name desc [] showHelp

-- -- | Generates a 'KeyBind' without a name or description.
-- -- See 'withName' and 'withDesc' if you want to add one, or 'keyBindEvent' if you want to add both.
-- fromEvent :: EventM n s () -> KeyBind n s
-- fromEvent handle = KeyBind handle Nothing Nothing []
--
-- -- | Adds a name to a 'KeyBind', overwriting the previous name if it existed.
-- withName :: Text -> KeyBind n s -> KeyBind n s
-- withName name (KeyBind h _n d ks) = KeyBind h (Just name) d ks
--
-- -- | Adds a description to a 'KeyBind', overwriting the previous description if it existed.
-- withDesc :: Text -> KeyBind n s -> KeyBind n s
-- withDesc desc (KeyBind h n _d ks) = KeyBind h n (Just desc) ks

-- | For internal use (see 'keybinds'), you shouldn't need to modify the keys yourself.
addKey :: KeyCode -> KeyBind n s -> KeyBind n s
addKey key (KeyBind h n d keys sh) = KeyBind h n d (key:keys) sh

-- -- | Specializes 'BrickEvent' to exclude 'AppEvent's. There's no reason we couldn't
-- -- support them, but they aren't user input events so it doesn't make sense to.
-- type UserInput n = BrickEvent n ()

data KeyBindings id n s = KeyBindings
  { keybindingsIdToBinding  :: Map id (KeyBind n s) -- ^ One-to-one
  , keybindingsKeyToId      :: Map KeyCode id -- ^ Possibly many-to-one
  , keyBindingsDefaultEvent :: Maybe (EventM n s ()) -- ^ The default event to use, uses pure () if not given.
  }

-- | Smart constructor that populates the 'kbKeys' of each 'KeyBind'.
keybindings :: Ord id => Map KeyCode id -> Map id (KeyBind n s) -> Maybe (EventM n s ()) -> KeyBindings id n s
keybindings keyToId idToBinding defaultEvent = KeyBindings idToBindingWithKeys keyToId defaultEvent
  where
    idToBindingWithKeys = M.foldrWithKey' (M.adjust . addKey) idToBinding keyToId

-- Adding and removing keybinds is currently unsupported although it wouldn't be hard
-- to add.
-- addKeyBind :: Id -> UserInput n -> KeyBind n s -> KeyBinds n s -> KeyBinds n s

-- | For internal use.
-- stripAppEvent :: BrickEvent n e -> UserInput n
-- stripAppEvent event = case event of
--   AppEvent _ -> AppEvent ()
--   VtyEvent e -> VtyEvent e
--   MouseDown n b ms l -> MouseDown n b ms l
--   MouseUp n b l -> MouseUp n b l

drawHelpMenu :: KeyBindings id n s -> Widget n
drawHelpMenu kbs = centerLayer . border $ header <=> vBox keyBinds
  where
    header = str "Key Bindings"
    keyBinds = map drawKeyBind $ M.elems (keybindingsIdToBinding kbs)

drawKeyBind :: KeyBind n s -> Widget n
drawKeyBind KeyBind{..}
  | not kbShowHelp = emptyWidget
drawKeyBind KeyBind{..} = (keybinds <+> str ": " <+> name) <=> desc
  where
    name = maybe emptyWidget txt kbName
    desc = maybe emptyWidget txt kbDesc
    keybinds = str . unwords $ mapMaybe showKey kbKeys
    showKey = fmap wrapWithBrackets . showKeyCode
    wrapWithBrackets s = "[" <> s <> "]"

appEventKeyBinds :: Ord id => KeyBindings id n s -> KeyCode -> EventM n s ()
appEventKeyBinds (KeyBindings events ids defaultEventMaybe) event =
  case M.lookup event ids of
    Just ident -> maybe defaultEvent kbEvent $ M.lookup ident events
    Nothing -> defaultEvent
  where
    defaultEvent = fromMaybe (pure ()) defaultEventMaybe

-- TODO: Make IdToBinding an ordered map.
-- TODO: Figure out how to support key bindings in a named context (i.e. BrickEvent n ()).
