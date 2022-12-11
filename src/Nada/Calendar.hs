{-# LANGUAGE RecordWildCards #-}
module Nada.Calendar
  (
    drawCalendar
  , appEventCalendar
  , CalendarState(..)
  , makeCalendarStateForCurrentDay
  -- * Exposed for testing
  , makeEmptyCalendarStateFromDay
  , calendarBlockRange
  , firstMondayBefore
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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time (getZonedTime, zonedTimeToLocalTime, localDay)
import Data.Time.Calendar
import Data.Time.Calendar.Month
import Data.Time.Format

import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as E

import Lens.Micro ( (%~), ASetter' )

data CalendarState resourceName
  = CalendarState
  { calendarSelectedDate  :: Day
  , calendarSelectedMonth :: Month
  , calendarWidgets :: Map Day [Widget resourceName]
  }

makeEmptyCalendarStateFromDay :: Day -> CalendarState n
makeEmptyCalendarStateFromDay day = CalendarState day (dayPeriod day) mempty

makeCalendarStateForCurrentDay :: IO (CalendarState n)
makeCalendarStateForCurrentDay = do
  now <- getZonedTime
  let day = localDay $ zonedTimeToLocalTime now
  pure $ makeEmptyCalendarStateFromDay day
 

drawCalendar :: Ord n => n -> (Day -> n) -> CalendarState n -> Widget n
drawCalendar _calendarName dayToName state@CalendarState{..} = header <=> drawCalendarBody dayToName state <=> footer
  where
    YearMonth yearNumber _ = calendarSelectedMonth
    monthName = formatTime defaultTimeLocale "%B" calendarSelectedMonth
    header = str monthName <+> str " " <+> str (show yearNumber)
    footer = str "[j]: Down [k]: Up [h]: Left [l]: Right [C-u]: Prev month [C-d]: Next month [q/<Esc>]: Exit"

drawCalendarBody :: Ord n => (Day -> n) -> CalendarState n -> Widget n
drawCalendarBody dayToName state@CalendarState{..} = joinBorders . vBox $
  weekHeader 
  : map (drawWeek dayToName state) weeks
  where
    weekHeader = hBox $ map (border . hCenter . str)
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
drawWeek dayToName state days = hBox $ map (drawDay dayToName state) days

drawDay :: Ord n => (Day -> n) -> CalendarState n -> Day -> Widget n
drawDay dayToName CalendarState{..} day = addSelectionHighlight . border . clickable widgetName $
  case widgets of
    Nothing -> dayWidget
    Just ws -> dayWidget <=> ws
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
    widgets = vBox <$> M.lookup day calendarWidgets
    widgetName = dayToName day

-- | Gets the first Monday before the date. Returns the date if it is a Monday.
firstMondayBefore :: Day -> Day
firstMondayBefore day = case dayOfWeek day of
  Monday -> day
  _      -> firstMondayBefore (pred day)

calendarBlockRange :: Month -> (Day, Day)
-- We will at least add 27 to accommodate the shortest months, but we might need
-- to add more to accommodate longer months.
calendarBlockRange month = (firstMonday, lastDate (addDays 27 firstMonday))
  where
    firstMonday = firstMondayBefore $ periodFirstDay month
    lastOfMonth = periodLastDay month
    lastDate d
      | d >= lastOfMonth = d
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

-- | Adjusts the day and changes the selected month if the day is out of bounds.
adjustDay :: (Day -> Day) -> CalendarState n -> CalendarState n
adjustDay adjustment state@CalendarState{..} = if adjustedDay < currentBlockStart || adjustedDay > currentBlockEnd
  then
    -- Note that it does not suffice to always set the selected month to the
    -- corresponding month of 'adjustedDay' because block ranges generally
    -- contain multiple months.
    state{calendarSelectedDate = adjustedDay, calendarSelectedMonth = dayPeriod adjustedDay}
  else
    state{calendarSelectedDate = adjustedDay}
  where
    (currentBlockStart, currentBlockEnd) = calendarBlockRange calendarSelectedMonth
    adjustedDay = adjustment calendarSelectedDate

-- | Does not change the selected day
prevSelectedMonth :: CalendarState n -> CalendarState n
prevSelectedMonth state@CalendarState{..} = state{calendarSelectedMonth = pred calendarSelectedMonth}

-- | Does not change the selected day
nextSelectedMonth :: CalendarState n -> CalendarState n
nextSelectedMonth state@CalendarState{..} = state{calendarSelectedMonth = succ calendarSelectedMonth}

-- FIXME: Need to keep the same relative location in the calendarBlock
-- nextMonth :: CalendarState n -> CalendarState n
-- nextMonth state@CalendarState{..} = state{calendarSelectedMonth = succ calendarSelectedMonth, calendarSelectedDate = addDays 28 calendarSelectedDate}
-- 
-- prevMonth :: CalendarState n -> CalendarState n
-- prevMonth state@CalendarState{..} = state{calendarSelectedMonth = pred calendarSelectedMonth, calendarSelectedDate = addDays (-28) calendarSelectedDate}

prevWeek :: CalendarState n -> CalendarState n
prevWeek = adjustDay (addDays (-7))

nextWeek :: CalendarState n -> CalendarState n
nextWeek = adjustDay (addDays 7)

prevDay :: CalendarState n -> CalendarState n
prevDay = adjustDay pred

nextDay :: CalendarState n -> CalendarState n
nextDay = adjustDay succ

appEventCalendar :: ASetter' s (CalendarState n) -> (n -> Maybe Day) -> EventM n s () -> BrickEvent n e -> EventM n s ()
-- Select cell
appEventCalendar calendarState nameToDay _ (MouseDown n E.BLeft _ _) = case nameToDay n of
  Just day -> modify (calendarState %~ \state -> state{calendarSelectedDate = day})
  Nothing  -> pure ()
-- Scroll through weeks
appEventCalendar calendarState _ _ (MouseDown _ E.BScrollDown _ _) =
  modify $ calendarState %~ nextSelectedMonth
appEventCalendar calendarState _ _ (MouseDown _ E.BScrollUp   _ _) =
  modify $ calendarState %~ prevSelectedMonth
-- Move across days
appEventCalendar calendarState _ exitCalendar (VtyEvent vtyE) = case vtyE of
  V.EvKey (V.KChar 'j') [] ->
    modify $ calendarState %~ nextWeek
  V.EvKey (V.KChar 'k') [] ->
    modify $ calendarState %~ prevWeek
  V.EvKey (V.KChar 'h') [] ->
    modify $ calendarState %~ prevDay
  V.EvKey (V.KChar 'l') [] ->
    modify $ calendarState %~ nextDay
  V.EvKey (V.KChar 'd') [V.MCtrl] ->
    modify $ calendarState %~ nextSelectedMonth
  V.EvKey (V.KChar 'u') [V.MCtrl] ->
    modify $ calendarState %~ prevSelectedMonth
  V.EvKey (V.KChar 'q') [] ->
    exitCalendar
  V.EvKey V.KEsc [] ->
    exitCalendar
  _ -> pure ()
appEventCalendar _ _ _ _ = pure ()
