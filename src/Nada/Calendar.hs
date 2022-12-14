{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Nada.Calendar
  (
    drawCalendar
  , appEventCalendar
  , appEventCalendarLens
  , CalendarState(..)
  , CalendarName(..)
  , CalendarNameConverter(..)
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
import Data.List (intersperse)
import Data.Time (getZonedTime, zonedTimeToLocalTime, localDay)
import Data.Time.Calendar
import Data.Time.Calendar.Month
import Data.Time.Format

import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as E

import Lens.Micro ( (%~), (^.), Lens', lens)

data CalendarName
  = CalendarBody -- ^ As of now, this is unused.
  | CalendarDate Day -- ^ A date, rendered as a cell in the calendar grid.
  | CalendarMonth MonthOfYear -- ^ The month, rendered on the top left.
  | CalendarYear Year -- ^ The year, rendered on the top left.
  | CalendarNextMonth -- ^ The next month button, rendered on the top right.
  | CalendarPrevMonth -- ^ The prev month button, rendered on the top right.
  deriving (Show, Eq, Ord)

data CalendarNameConverter resourceName
  = CalendarNameConverter
  { toResourceName :: CalendarName -> resourceName
  , matchCalendarName :: resourceName -> Maybe CalendarName
  }

data CalendarState resourceName
  = CalendarState
  { calendarSelectedDate  :: Day
  , calendarSelectedMonth :: Month
  , calendarWidgets :: Day -> [Widget resourceName]
  , calendarNameConverter :: CalendarNameConverter resourceName
  }

makeEmptyCalendarStateFromDay :: CalendarNameConverter n -> Day -> CalendarState n
makeEmptyCalendarStateFromDay cn day = CalendarState day (dayPeriod day) mempty cn

makeCalendarStateForCurrentDay :: CalendarNameConverter n -> IO (CalendarState n)
makeCalendarStateForCurrentDay cn = do
  now <- getZonedTime
  let day = localDay $ zonedTimeToLocalTime now
  pure $ makeEmptyCalendarStateFromDay cn day

drawCalendar :: Ord n => CalendarState n -> Widget n
drawCalendar state@CalendarState{..} = header <=> drawCalendarBody state <=> footer
  where
    YearMonth y m = calendarSelectedMonth
    monthStr = formatTime defaultTimeLocale "%B" calendarSelectedMonth
    yearStr = formatTime defaultTimeLocale "%Y" calendarSelectedMonth
    monthName = toResourceName calendarNameConverter . CalendarMonth $ m
    yearName = toResourceName calendarNameConverter . CalendarYear $ y
    -- Ensures all months are of the same visual length and
    -- that the year doesn't get cut off by the month selector
    month = clickable monthName . hLimit 9 $ str monthStr
    year = clickable yearName $ str yearStr
    monthYear = str " " <+> month <+> str " " <+> year
    prevMonthName = toResourceName calendarNameConverter CalendarPrevMonth
    nextMonthName = toResourceName calendarNameConverter CalendarNextMonth
    monthButtons = clickable prevMonthName (str "←") <+> str "  " <+> clickable nextMonthName (str "→")
    -- Ensures that the fills do not occupy vertical space
    header = vLimit 1 (monthYear <+> fill ' ' <+> monthButtons)
    footer = str "[j]: Down [k]: Up [h]: Left [l]: Right [C-u]: Prev month [C-d]: Next month [q/<Esc>]: Exit"

drawCalendarBody :: Ord n => CalendarState n -> Widget n
drawCalendarBody state@CalendarState{..} = joinBorders . border . vBox . intersperse hBorder $
  weekHeader 
  : map (drawWeek state) weeks
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

drawWeek :: Ord n => CalendarState n -> [Day] -> Widget n
drawWeek state days = hBox . intersperse vBorder $ map (drawDay state) days

drawDay :: Ord n => CalendarState n -> Day -> Widget n
drawDay CalendarState{..} day = addSelectionHighlight . clickable widgetName $
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
    widgetName = toResourceName calendarNameConverter . CalendarDate $ day

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

prevSelectedYear :: CalendarState n -> CalendarState n
prevSelectedYear state@CalendarState{..} = state{calendarSelectedMonth = addMonths (-12) calendarSelectedMonth}

nextSelectedYear :: CalendarState n -> CalendarState n
nextSelectedYear state@CalendarState{..} = state{calendarSelectedMonth = addMonths 12 calendarSelectedMonth}

-- | Does not change the selected day
prevSelectedMonth :: CalendarState n -> CalendarState n
prevSelectedMonth state@CalendarState{..} = state{calendarSelectedMonth = pred calendarSelectedMonth}

-- | Does not change the selected day
nextSelectedMonth :: CalendarState n -> CalendarState n
nextSelectedMonth state@CalendarState{..} = state{calendarSelectedMonth = succ calendarSelectedMonth}

nextYear :: CalendarState n -> CalendarState n
nextYear = adjustMonth (addMonths 12)

prevYear :: CalendarState n -> CalendarState n
prevYear = adjustMonth (addMonths (-12))

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

handleMouseCalendar :: Lens' s (CalendarState n) -> EventM n s () -> n -> V.Button -> [V.Modifier] -> EventM n s ()
handleMouseCalendar calendarState _exitCalendar n button modifiers = do
  CalendarNameConverter{..} <- calendarNameConverter <$> gets (^. calendarState)
  case (button, modifiers) of
    (E.BLeft, _) ->
      case matchCalendarName n of
        Just (CalendarDate day) -> modify (calendarState %~ \state -> state{calendarSelectedDate = day})
        Just CalendarNextMonth  -> modify (calendarState %~ nextSelectedMonth)
        Just CalendarPrevMonth  -> modify (calendarState %~ prevSelectedMonth)
        _ -> pure ()
    (E.BScrollDown, [V.MShift]) -> modify (calendarState %~ nextSelectedYear)
    (E.BScrollDown, _) ->
      case matchCalendarName n of
        Just (CalendarYear _) -> modify (calendarState %~ nextSelectedYear)
        _ -> modify $ calendarState %~ nextSelectedMonth
    (E.BScrollUp, [V.MShift]) -> modify (calendarState %~ prevSelectedYear)
    (E.BScrollUp, _) ->
      case matchCalendarName n of
        Just (CalendarYear _) -> modify (calendarState %~ prevSelectedYear)
        _ -> modify $ calendarState %~ prevSelectedMonth
    _ -> pure ()

handleKeyPressCalendar :: Lens' s (CalendarState n) -> EventM n s () -> V.Event -> EventM n s ()
handleKeyPressCalendar calendarState exitCalendar vtyEvent = case vtyEvent of
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
  V.EvKey (V.KChar 'f') [V.MCtrl] ->
    modify $ calendarState %~ nextYear
  V.EvKey (V.KChar 'b') [V.MCtrl] ->
    modify $ calendarState %~ prevYear
  V.EvKey (V.KChar 'q') [] ->
    exitCalendar
  V.EvKey V.KEsc [] ->
    exitCalendar
  _ -> pure ()

-- | Specialized version of 'appEventCalendar' that uses a lens instead of a getter and setter.
appEventCalendarLens :: Lens' s (CalendarState n) -> EventM n s () -> BrickEvent n e -> EventM n s ()
appEventCalendarLens calendarState exitCalendar event = case event of
  MouseDown n button modifiers _ -> handleMouseCalendar calendarState exitCalendar n button modifiers
  VtyEvent vtyEvent -> handleKeyPressCalendar calendarState exitCalendar vtyEvent
  _ -> pure ()

appEventCalendar :: (s -> CalendarState n) -> (s -> CalendarState n -> s) -> EventM n s () -> BrickEvent n e -> EventM n s ()
appEventCalendar getCalendar setCalendar = appEventCalendarLens (lens getCalendar setCalendar)
