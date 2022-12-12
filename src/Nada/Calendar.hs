{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Nada.Calendar
  (
    drawCalendar
  , appEventCalendar
  , appEventCalendarLens
  , CalendarState(..)
  , makeCalendarStateForCurrentDay
  -- * Exposed for testing
  , makeEmptyCalendarStateFromDay
  , calendarBlockRange
  , firstDayOfWeekOnBefore
  , prevSelectedMonth
  , nextSelectedMonth
  , prevWeek
  , nextWeek
  , prevDay
  , nextDay
  ) where

import Brick
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border
import Data.Time (getZonedTime, zonedTimeToLocalTime, localDay)
import Data.Time.Calendar
import Data.Time.Calendar.Month
import Data.Time.Format

import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as E

import Lens.Micro ( (%~), ASetter', sets)
import Data.List (intersperse)

data CalendarState resourceName
  = CalendarState
  { calendarSelectedDate  :: Day
  , calendarSelectedMonth :: Month
  , calendarWidgets :: Day -> [Widget resourceName]
  }

makeEmptyCalendarStateFromDay :: Day -> CalendarState n
makeEmptyCalendarStateFromDay day = CalendarState day (dayPeriod day) mempty

makeCalendarStateForCurrentDay :: IO (CalendarState n)
makeCalendarStateForCurrentDay = do
  now <- getZonedTime
  let day = localDay $ zonedTimeToLocalTime now
  pure $ makeEmptyCalendarStateFromDay day
 

-- TODO: Make header clickable to adjust month/year + add keybindings for that
drawCalendar :: Ord n => n -> (Day -> n) -> CalendarState n -> Widget n
drawCalendar _calendarName dayToName state@CalendarState{..} = header <=> drawCalendarBody dayToName state <=> footer
  where
    YearMonth yearNumber _ = calendarSelectedMonth
    monthName = formatTime defaultTimeLocale "%B" calendarSelectedMonth
    header = str monthName <+> str " " <+> str (show yearNumber)
    footer = str "[j]: Down [k]: Up [h]: Left [l]: Right [C-u]: Prev month [C-d]: Next month [q/<Esc>]: Exit"

drawCalendarBody :: Ord n => (Day -> n) -> CalendarState n -> Widget n
drawCalendarBody dayToName state@CalendarState{..} = joinBorders . border . vBox . intersperse hBorder $
  weekHeader 
  : map (drawWeek dayToName state) weeks
  where
    weekHeader = vLimit 1 . hBox . intersperse vBorder $ map (hCenter . str)
      [ "MON"
      , "TUE"
      , "WED"
      , "THU"
      , "FRI"
      , "SAT"
      , "SUN"
      ]
    weeks = chunksOf 7 (calendarBlock calendarSelectedMonth)

drawWeek :: Ord n => (Day -> n) -> CalendarState n -> [Day] -> Widget n
drawWeek dayToName state days = hBox . intersperse vBorder $ map (drawDay dayToName state) days

drawDay :: Ord n => (Day -> n) -> CalendarState n -> Day -> Widget n
drawDay dayToName CalendarState{..} day = addSelectionHighlight . clickable widgetName $
    dayWidget <=> widgets <=> fill ' '
  where
    YearMonthDay _ _ dayOfMonth = day
    month = dayPeriod day
    monthShortName = formatTime defaultTimeLocale "%b" month
    dayWidget = if month == calendarSelectedMonth
      then hCenter . str $ show dayOfMonth
      else hCenter . str $ monthShortName <> " " <> show dayOfMonth
    addSelectionHighlight
      | calendarSelectedDate == day = forceAttr $ attrName "selected"
      | otherwise = id
    widgets = vBox $ calendarWidgets day
    widgetName = dayToName day

-- Modified defintion of 'firstDayOfWeekOnAfter'.
firstDayOfWeekOnBefore :: DayOfWeek -> Day -> Day
firstDayOfWeekOnBefore dw d = addDays (negate . toInteger $ dayOfWeekDiff (dayOfWeek d) dw) d

calendarBlockRange :: Month -> (Day, Day)
-- We will at least add 27 to accommodate the shortest months, but we might need
-- to add more to accommodate longer months.
calendarBlockRange month = (firstMonday, lastDate (addDays 27 firstMonday))
  where
    firstMonday = firstDayOfWeekOnBefore Monday $ periodFirstDay month
    lastDate d
      | d >= periodLastDay month = d
      | otherwise = lastDate (addDays 7 d)

calendarBlock :: Month -> [Day]
calendarBlock month = enumFromTo startDay endDay
  where
    (startDay, endDay) = calendarBlockRange month

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = chunk : chunksOf n rest
  where
    (chunk, rest) = splitAt n xs

indexInCalendarBlockRange :: Day -> Month -> Integer
indexInCalendarBlockRange day month = diffDays day start
  where
    (start, _end) = calendarBlockRange month

dayFromCalendarBlockRangeIndex :: Month -> Integer -> Day
dayFromCalendarBlockRangeIndex month i = addDays i start
  where
    (start, _end) = calendarBlockRange month

-- | To be used in 'nextMonth' and 'prevMonth'.
-- The idea here is that if your date is the last Thursday (index 38) in a block with 6 weeks = 42 days
-- but you want to skip to a block with 3 weeks = 35 days, the natural thing to do is to select the
-- last Thursday of the next block (index 31). The other option is to simply clamp at the maximum, but
-- that would look weird.
clampIndexToBlockRange :: Month -> Integer -> Integer
clampIndexToBlockRange month = clampIndex
  where
    (start, end) = calendarBlockRange month
    blockRangeMaxIndex = diffDays end start
    clampIndex i
      | i <= blockRangeMaxIndex = i
      | otherwise = clampIndex (i - 7)

-- | Adjusts the day. If it exits the current calendar block, clamps to the
-- start/end of the preceding/following calendar block, respectively. Meant to
-- only be used in 'prevDay' and 'nextDay'.
adjustDay :: (Day -> Day) -> CalendarState n -> CalendarState n
adjustDay adjustment state@CalendarState{..} =
  if
    | adjustedDay < currentBlockStart -> state{calendarSelectedDate = newBlockEnd, calendarSelectedMonth = newMonth}
    | adjustedDay > currentBlockEnd   -> state{calendarSelectedDate = newBlockStart, calendarSelectedMonth = newMonth}
    | otherwise                       -> state{calendarSelectedDate = adjustedDay}
  where
    (currentBlockStart, currentBlockEnd) = calendarBlockRange calendarSelectedMonth
    adjustedDay = adjustment calendarSelectedDate
    newMonth = dayPeriod adjustedDay
    (newBlockStart, newBlockEnd) = calendarBlockRange newMonth

-- | Adjusts the week. If it exits the current calendar block, finds the first/last
-- day with the same day of the week as the current selected date. This ensures that
-- movement is seamless between blocks.
adjustWeek :: (Day -> Day) -> CalendarState n -> CalendarState n
adjustWeek adjustment state@CalendarState{..} =
  if
    | adjustedDay < currentBlockStart -> state{ calendarSelectedDate =
                                                firstDayOfWeekOnBefore currentDayOfWeek newBlockEnd
                                              , calendarSelectedMonth = newMonth
                                              }
    | adjustedDay > currentBlockEnd   -> state{ calendarSelectedDate =
                                                firstDayOfWeekOnAfter  currentDayOfWeek newBlockStart
                                              , calendarSelectedMonth = newMonth
                                              }
    | otherwise                       -> state{ calendarSelectedDate = adjustedDay }
  where
    (currentBlockStart, currentBlockEnd) = calendarBlockRange calendarSelectedMonth
    currentDayOfWeek = dayOfWeek calendarSelectedDate
    adjustedDay = adjustment calendarSelectedDate
    newMonth = dayPeriod adjustedDay
    (newBlockStart, newBlockEnd) = calendarBlockRange newMonth

-- | Changes both selected month and selected day, attempting to preserve the
-- index of the day in the block range (not the date itself).  Keeps the same
-- day of the week when selected date's relative index exceeds the bounds of the
-- next month (see 'clampIndexToBlockRange').
adjustMonth :: (Month -> Month) -> CalendarState n -> CalendarState n
adjustMonth adjustment state@CalendarState{..} = state{calendarSelectedMonth = newMonth, calendarSelectedDate = newDay}
  where
    newMonth = adjustment calendarSelectedMonth
    newIndex = clampIndexToBlockRange newMonth $ indexInCalendarBlockRange calendarSelectedDate calendarSelectedMonth
    newDay = dayFromCalendarBlockRangeIndex newMonth newIndex

-- | Does not change the selected day
prevSelectedMonth :: CalendarState n -> CalendarState n
prevSelectedMonth state@CalendarState{..} = state{calendarSelectedMonth = pred calendarSelectedMonth}

-- | Does not change the selected day
nextSelectedMonth :: CalendarState n -> CalendarState n
nextSelectedMonth state@CalendarState{..} = state{calendarSelectedMonth = succ calendarSelectedMonth}

nextMonth :: CalendarState n -> CalendarState n
nextMonth = adjustMonth succ

prevMonth :: CalendarState n -> CalendarState n
prevMonth = adjustMonth pred

prevWeek :: CalendarState n -> CalendarState n
prevWeek = adjustWeek (addDays (-7))

nextWeek :: CalendarState n -> CalendarState n
nextWeek = adjustWeek (addDays 7)

prevDay :: CalendarState n -> CalendarState n
prevDay = adjustDay pred

nextDay :: CalendarState n -> CalendarState n
nextDay = adjustDay succ

-- | Specialized version of 'appEventCalendar' that uses a lens instead of a getter and setter.
appEventCalendarLens :: ASetter' s (CalendarState n) -> (n -> Maybe Day) -> EventM n s () -> BrickEvent n e -> EventM n s ()
-- Select cell
appEventCalendarLens calendarState nameToDay _ (MouseDown n E.BLeft _ _) = case nameToDay n of
  Just day -> modify (calendarState %~ \state -> state {calendarSelectedDate = day})
  Nothing -> pure ()
-- Scroll through weeks
appEventCalendarLens calendarState _ _ (MouseDown _ E.BScrollDown _ _) =
  modify $ calendarState %~ nextSelectedMonth
appEventCalendarLens calendarState _ _ (MouseDown _ E.BScrollUp _ _) =
  modify $ calendarState %~ prevSelectedMonth
-- Move across days
appEventCalendarLens calendarState _ exitCalendar (VtyEvent vtyE) = case vtyE of
  V.EvKey (V.KChar 'j') [] ->
    modify $ calendarState %~ nextWeek
  V.EvKey (V.KChar 'k') [] ->
    modify $ calendarState %~ prevWeek
  V.EvKey (V.KChar 'h') [] ->
    modify $ calendarState %~ prevDay
  V.EvKey (V.KChar 'l') [] ->
    modify $ calendarState %~ nextDay
  V.EvKey (V.KChar 'd') [V.MCtrl] ->
    modify $ calendarState %~ nextMonth
  V.EvKey (V.KChar 'u') [V.MCtrl] ->
    modify $ calendarState %~ prevMonth
  V.EvKey (V.KChar 'q') [] ->
    exitCalendar
  V.EvKey V.KEsc [] ->
    exitCalendar
  _ -> pure ()
appEventCalendarLens _ _ _ _ = pure ()

appEventCalendar :: (s -> CalendarState n) -> (CalendarState n -> s) -> (n -> Maybe Day) -> EventM n s () -> BrickEvent n e -> EventM n s ()
appEventCalendar getCalendar setCalendar = appEventCalendarLens calendarState
  where
    calendarState = sets $ \modifyCalendar s -> setCalendar (modifyCalendar $ getCalendar s)
