module Nada.KeyBinds.IniParser
  (
    parseKeyBinds
  , parseKeyBindsFromFile
  ) where

import Nada.KeyBinds.KeyCodes (parseKeyCode, KeyCode)
import Data.Ini.Config
import Data.List (nub, (\\))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty.Input.Events

type Id = Text

parseField :: Text -> Id -> SectionParser (Text, [KeyCode])
parseField separator identifier = do
  keyCodesMaybe <- fieldMbOf identifier (listWithSeparator separator string)
  -- Any malformed keycodes are ignored. I would prefer to raise an
  -- error but I don't see a way to do so using 'Data.Ini.Config'.
  -- The proper way to circumvent this would be to do your parsing in
  -- two stages.
  pure (identifier, maybe [] (mapMaybe (parseKeyCode . T.unpack)) keyCodesMaybe)

duplicates :: Eq a => [a] -> [a]
duplicates as = as \\ nub as

-- | Use this parser if you already have an INI file you're reading configs
-- from. Otherwise, try 'parseKeyBindsFromFile'.
--
-- Requires a 'sectionName' for the section in which the keybinds are contained,
-- a 'separator' to separate the keybinds on each line, and the list of
-- 'identifiers' that you allow keybinds for. It is up to you to enforce
-- that the 'identifiers' you provide have matching handlers in your
-- 'KeyBindings'.
--
-- The '[KeyCode]' in the tuple is a list of all duplicated 'KeyCode's. This
-- should probably result in a parse failure because the last 'Id' corresponding
-- to a duplicated 'KeyCode' is what it will be bound to; however, in the interest
-- of not being prescriptive this is left to the user.
parseKeyBinds :: Text -> Text -> [Id] -> IniParser (Maybe (Map KeyCode Id, [KeyCode]))
parseKeyBinds sectionName separator identifiers = sectionMb sectionName $ do
  entries <- traverse (parseField separator) identifiers
  let duplicateKeyCodes = duplicates $ concatMap snd entries
      keyCodeToId = M.fromList
                      -- We need to expand 'entries :: [(Text, [KeyCode])]' to [(KeyCode, Text)]
                      -- in order to make it into a map.
                      [(keyCode, identifier) | (identifier, keyCodes) <- entries, keyCode <- keyCodes]
  pure (keyCodeToId, duplicateKeyCodes)

-- | Parses keybinds from the contents of the INI file 'file'. Uses 'parseKeyBinds'.
-- If there isn't anything to parse (e.g. the file is empty or the
-- keybinds are in a different section) returns '(Map.empty, [])'.
parseKeyBindsFromFile :: Text -> Text -> Text -> [Id] -> Either String (Map Event Id, [KeyCode])
parseKeyBindsFromFile file sectionName separator identifiers = do
  resultMaybe <- parseIniFile file (parseKeyBinds sectionName separator identifiers)
  pure $ fromMaybe mempty resultMaybe
