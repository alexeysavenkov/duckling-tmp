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

distances :: [(Text, String, TDistance.Unit)]
distances = [ ("<latent dist> km", "км|кілометр(а|ів)?", TDistance.Kilometre)
            , ("<latent dist> feet", "('|фут(а|ів|ах)?)", TDistance.Foot)
            , ("<latent dist> inch", "(\"|''|дюйм(а|ів|ах)?)", TDistance.Inch)
            , ("<latent dist> yard", "ярд(а|ів|ах)?", TDistance.Yard)
            , ("<dist> meters", "м(етр(а|ів|ах)?)?", TDistance.Metre)
            , ("<dist> centimeters", "см|сантиметр(а|ів|ах)?", TDistance.Centimetre)
            , ("<dist> millimeters", "мм|міліметр(а|ів|ах)?", TDistance.Millimetre)
            , ("<dist> miles", "мил(я|і|ь|ях)", TDistance.Mile)
            ]

ruleDistances :: [Rule]
ruleDistances = map go distances
  where
    go :: (Text, String, TDistance.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [ regex regexPattern ]
      , prod = \tokens -> case tokens of
          (Token Distance dd:_) -> Just . Token Distance $ withUnit u dd
          _ -> Nothing
      }

rules :: [Rule]
rules = ruleDistances
