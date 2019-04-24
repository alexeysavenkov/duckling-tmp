{-# LANGUAGE GADTs #-}

module Duckling.AmountOfMoney.UK.Helpers
  ( isUAHCents
  , withUAHCents
  , isWithoutUAHCents
  )
  where

import Duckling.AmountOfMoney.Types
  ( Currency (..)
  , AmountOfMoneyData (..)
  , amountOfMoneyData'
  )
import Duckling.Numeral.Types (getIntValue, isInteger)
import Duckling.Dimensions.Types
import Duckling.Types hiding (Entity(..))


isUAHCents :: Predicate
isUAHCents (Token AmountOfMoney AmountOfMoneyData{value = Just _, currency = UAHCent}) = True
isUAHCents _ = False

withUAHCents :: Double -> AmountOfMoneyData -> AmountOfMoneyData
withUAHCents x fd@AmountOfMoneyData {value = Just value} = fd
  {value = Just $ value + x / 100}
withUAHCents x AmountOfMoneyData {value = Nothing} =
  amountOfMoneyData'{value = Just x, currency = UAHCent}

isWithoutUAHCents :: Predicate
isWithoutUAHCents (Token AmountOfMoney AmountOfMoneyData{currency = UAHCent}) = False
isWithoutUAHCents (Token AmountOfMoney AmountOfMoneyData{value = Just v}) = isInteger v
isWithoutUAHCents _ = False