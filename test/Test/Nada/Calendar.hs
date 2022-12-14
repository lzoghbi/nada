{-# LANGUAGE RecordWildCards #-}
module Test.Nada.Calendar
  (
    calendarTests
  ) where

import Nada.Calendar

import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.Month
import Data.Time.Calendar.OrdinalDate

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Nada.Calendar (firstDayOfWeekOnBefore)

newtype TestCalendarState n = TestCalendarState (CalendarState n)

instance Show (TestCalendarState n) where
  show (TestCalendarState CalendarState{..})
    = "(Selected month: " <> show calendarSelectedMonth 
      <> ", Selected date: " <> show calendarSelectedDate <> ")"

genDay :: QC.Gen Day
genDay = do
  year <- chooseInteger (1, 10000)
  let lastDay = if isLeapYear year
                  then 366
                  else 365
  day <- chooseInt (1, lastDay)
  pure $ YearDay year day

genMonth :: QC.Gen Month
genMonth = MkMonth <$> chooseInteger (1, 100000)

genCalendarState :: QC.Gen (TestCalendarState n)
genCalendarState = TestCalendarState . makeEmptyCalendarStateFromDay <$> genDay

propFirstDayOfWeekOnBeforeValid :: Property
propFirstDayOfWeekOnBeforeValid = forAll genDay $ \day ->
  let monday = firstDayOfWeekOnBefore Monday day
   in -- Is a Monday
      dayOfWeek monday == Monday
      -- Is the first preceding
      && diffDays day monday < 7

propCalendarBlockRangeValid :: Property
propCalendarBlockRangeValid = forAll genMonth $ \month ->
  let (start, end) = calendarBlockRange month
   in -- Diff is a multiple of 7 minus 1
      ((diffDays end start) + 1) `mod` 7 == 0
      -- Block contains all days in the month
      && (periodFirstDay month) >= start
      && (periodLastDay month) <= end

csEqual :: CalendarState n -> CalendarState n -> Bool
csEqual cs1 cs2 =
  (calendarSelectedDate cs1) == (calendarSelectedDate cs2)
  && (calendarSelectedMonth cs1) == (calendarSelectedMonth cs2)

propPrevNextSelectedMonthInverse :: Property
propPrevNextSelectedMonthInverse = forAll genCalendarState $ \(TestCalendarState cs) ->
  cs `csEqual` ((prevSelectedMonth . nextSelectedMonth) $ cs)

propPrevNextMonthDoesNotChangeDay :: Property
propPrevNextMonthDoesNotChangeDay = forAll genCalendarState $ \(TestCalendarState cs) ->
  (calendarSelectedDate cs) == (calendarSelectedDate . nextSelectedMonth $ cs)

propPrevNextWeekInverse :: Property
propPrevNextWeekInverse = forAll genCalendarState $ \(TestCalendarState cs) ->
  (calendarSelectedDate cs) == (calendarSelectedDate . prevWeek . nextWeek $ cs)

propPrevNextDayInverse :: Property
propPrevNextDayInverse = forAll genCalendarState $ \(TestCalendarState cs) ->
  (calendarSelectedDate cs) == (calendarSelectedDate . prevDay . nextDay $ cs)

propertyTests = testGroup "Property tests"
  [ QC.testProperty "firstDayOfWeekOnBefore" propFirstDayOfWeekOnBeforeValid
  , QC.testProperty "calendarBlockRange" propCalendarBlockRangeValid
  , QC.testProperty "prevSelectedMonth . nextSelectedMonth == id" propPrevNextSelectedMonthInverse
  , QC.testProperty "nextSelectedMonth does not change selected date" propPrevNextMonthDoesNotChangeDay
  , QC.testProperty "prevWeek . nextWeek does not change selected date" propPrevNextWeekInverse
  , QC.testProperty "prevDay . nextDay does not change selected date" propPrevNextDayInverse
  ]

testCalendarState :: CalendarState n
-- Block range for this month is (Nov 28, Jan 1)
testCalendarState = makeEmptyCalendarStateFromDay (YearMonthDay 2022 12 3)

unitTests = testGroup "Unit tests"
  [ testCase "Selected month decreases when going below block range" $
      calendarSelectedMonth (prevWeek testCalendarState) @?= pred (calendarSelectedMonth testCalendarState)
  , testCase "Selected month does not change when within block range" $
      calendarSelectedMonth (prevDay testCalendarState) @?= calendarSelectedMonth testCalendarState
  ]

calendarTests = testGroup "Nada.Calendar" [ propertyTests, unitTests ]
