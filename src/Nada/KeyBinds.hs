{-# LANGUAGE RecordWildCards #-}
module Nada.KeyBinds
  ( -- * 'KeyBind'
    KeyBind
  , keyBind
    -- * 'KeyBinds'
  , KeyBindings
  , keybindings
  -- * Drawing
  , kbNameAttr
  , kbKeysAttr
  -- , kbDescAttr
  , drawHelpMenu
  , drawHelpMenuForIds
  -- * Handling events
  , appEventKeyBinds
  ) where

import Nada.KeyBinds.KeyCodes

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data KeyBind n s = KeyBind
  { kbEvent :: EventM n s ()
  , kbName :: Maybe Text -- ^ Optional: will not render in the help menu if not provided.
  -- , kbDesc :: Maybe Text -- ^ Optional.
  , kbKeys :: [KeyCode] -- ^ You should not have to set this yourself.
                           -- It should be set automatically 'keybinds'.
  }

-- | Smart constructor that creates a 'KeyBind'.
-- If you don't want to provide a name and description, see 'fromEvent'.
--
-- Initializes the 'kbKeys' to the empty list - it will be autopopulated in 'keybinds'.
keyBind :: EventM n s () -> Maybe Text -> KeyBind n s
keyBind handle name = KeyBind handle name []

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
addKey key (KeyBind h n keys) = KeyBind h n (key:keys)

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

kbNameAttr :: AttrName
kbNameAttr = attrName "keyBindName"

kbKeysAttr :: AttrName
kbKeysAttr = attrName "keyBindKeys"

-- kbDescAttr :: AttrName
-- kbDescAttr = attrName "keyBindDesc"

-- | Draws a help menu using the keys of 'keybindingsIdToBinding'. If you want
-- to control what gets shown or what order they appear, use 'drawHelpMenuForIds'.
drawHelpMenu :: Ord id => Widget n -> KeyBindings id n s -> Widget n
drawHelpMenu menuLabel kbs = drawHelpMenuForIds menuLabel (M.keys $ keybindingsIdToBinding kbs) kbs

-- | Draws a help menu for the 'ids' in the order they appear.
-- Uses 'menuLabel' as the label for the menu.
-- If an identifier is not matched in 'keybindingsIdToBinding',
-- we simply do not draw anything. Similarly, if the 'kbName' is 'Nothing',
-- we do not draw the keybinding.
--
-- I considered making it more customizable, but this function is pretty simple
-- (ignoring the alignemnt hacks) so if you want to do anything more custom with
-- rendering a help menu then please copy and extend or roll your own.
drawHelpMenuForIds :: Ord id => Widget n -> [id] -> KeyBindings id n s -> Widget n
drawHelpMenuForIds menuLabel ids kbs = borderWithLabel menuLabel menuEntries
  where
    keyBinds =
      -- Ignore any identifiers without a corresponding 'KeyBind'
      mapMaybe (\ident -> M.lookup ident (keybindingsIdToBinding kbs)) ids
    maxLength = foldr (\kb maxSoFar -> max maxSoFar $ maybe 0 T.length (kbName kb)) 0 keyBinds
    menuEntries = vBox $ map (drawKeyBind maxLength) keyBinds

-- | Left-aligns the names and key binds.
drawKeyBind :: Int -> KeyBind n s -> Widget n
drawKeyBind maxLength KeyBind{..} = case kbName of
  Nothing -> emptyWidget
  Just kbName' -> drawName kbName' <+> drawKeys kbKeys -- <=> drawDesc kbDesc
  where
    -- The +1 ensures that there's a gap between the name and keys
    drawName name = withAttr kbNameAttr .
      padRight (Pad $ 1 + maxLength - T.length name) $ txt name
    drawKeys = withAttr kbKeysAttr .
      str . unwords . mapMaybe showKey
    showKey = fmap wrapWithBrackets . showKeyCode
    wrapWithBrackets s = "[" <> s <> "]"
    -- drawDesc = withAttr kbDescAttr .
    --   maybe emptyWidget txt

appEventKeyBinds :: Ord id => KeyBindings id n s -> KeyCode -> EventM n s ()
appEventKeyBinds (KeyBindings events ids defaultEventMaybe) event =
  case M.lookup event ids of
    Just ident -> maybe defaultEvent kbEvent $ M.lookup ident events
    Nothing -> defaultEvent
  where
    defaultEvent = fromMaybe (pure ()) defaultEventMaybe

-- TODO: Figure out how to render description
-- TODO: Figure out how to support key bindings in a named context (i.e. BrickEvent n ()).
