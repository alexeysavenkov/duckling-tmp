-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.UK.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Distance.Types (DistanceData(..))
import Duckling.Types
import qualified Duckling.Distance.Types as TDistance
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral

ruleNumeralAsDistance :: Rule
ruleNumeralAsDistance = Rule
  { name = "number as distance"
  , pattern =
    [ dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData {TNumeral.value = v}:_) ->
        Just . Token Distance $ distance v
      _ -> Nothing
  }

distances :: [(Text, String, TDistance.Unit)]
distances = [ ("<latent dist> km", "км|кілометр(ах|ів|а)?", TDistance.Kilometre)
            , ("<latent dist> feet", "('|фут(ах|ів|а)?)", TDistance.Foot)
            , ("<latent dist> inch", "(\"|''|дюйм(ах|ів|а)?)", TDistance.Inch)
            , ("<latent dist> yard", "ярд(ах|ів|а)?", TDistance.Yard)
            , ("<dist> meters", "м(етр(ах|ів|а)?)?", TDistance.Metre)
            , ("<dist> centimeters", "см|сантиметр(ах|ів|а)?", TDistance.Centimetre)
            , ("<dist> millimeters", "мм|міліметр(ах|ів|а)?", TDistance.Millimetre)
            , ("<dist> miles", "мил(я|і|ь|ях)", TDistance.Mile)
            ]

ruleDistances :: [Rule]
ruleDistances = map go distances
  where
    go :: (Text, String, TDistance.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [ dimension Distance, regex regexPattern ]
      , prod = \tokens -> case tokens of
          (Token Distance dd:_) -> Just . Token Distance $ withUnit u dd
          _ -> Nothing
      }

rules :: [Rule]
rules = ruleNumeralAsDistance:ruleDistances
