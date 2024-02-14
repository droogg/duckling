-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Duckling.Ordinal.RU.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral

import Duckling.Ordinal.Types as TOrdinal
import Data.Text

ordinalsFirstthMap :: HashMap Text.Text Int
ordinalsFirstthMap = HashMap.fromList
  [ ( "перв", 1 )
  , ( "втор", 2 )
  , ( "трет", 3 )
  , ( "четверт", 4 )
  , ( "четвёрт", 4 )
  , ( "пят", 5 )
  , ( "шест", 6 )
  , ( "седьм", 7 )
  , ( "восьм", 8 )
  , ( "девят", 9 )
  , ( "десят", 10 )
  , ( "одиннадцат", 11 )
  , ( "двенадцат", 12 )
  , ( "тринадцат", 13 )
  , ( "четырнадцат", 14 )
  , ( "пятнадцат", 15 )
  , ( "шестнадцат", 16 )
  , ( "семнадцат", 17 )
  , ( "восемнадцат", 18 )
  , ( "девятнадцат", 19 )
  , ( "двадцат", 20 )
  , ( "тридцат", 30 )
  , ( "сороков", 40 )
  , ( "пятидесят", 50 )
  , ( "шестидесят", 60 )
  , ( "семидесят", 70 )
  , ( "восьмидесят", 80 )
  , ( "девяност", 90 )
  , ( "сот", 100 )
  ]

cardinalsMap :: HashMap Text.Text Int
cardinalsMap = HashMap.fromList
  [ ( "двадцать", 20 )
  , ( "тридцать", 30 )
  , ( "сорок", 40 )
  , ( "пятьдесят", 50 )
  , ( "шестьдесят", 60 )
  , ( "семьдесят", 70 )
  , ( "восемьдесят", 80 )
  , ( "девяносто", 90 )
  , ( "сто", 100 )
  , ( "двести", 200)
  , ( "триста", 300)
  , ( "четыреста", 400)
  , ( "пятьсот", 500)
  , ( "шестьсот", 600)
  , ( "семьсот", 700)
  , ( "восемьсот", 800)
  , ( "девятьсот", 900)
  ]

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..20th, then 30th, 40th, ..., 100th)"
  , pattern =
    [ regex "(перв|втор|трет|четв[её]рт|пят|шест|седьм|восьм|девят|десят|одиннадцат|двенадцат|тринадцат|четырнадцат|пятнадцат|шестнадцат|семнадцат|восемнадцат|девятнадцат|двадцат|тридцат|сороков|пятидесят|шестидесят|семидесят|восьмидесят|девяност|сот)(ь(его|ему|ей|ем|им|их|и|е)|ого|ому|ый|ой|ий|ая|ое|ья|ом|ые|ым|ых)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsFirstthMap
      _ -> Nothing
  }

ruleOrdinal :: Rule
ruleOrdinal = Rule
  { name = "ordinal 21..99"
  , pattern =
    [ regex "(двадцать|тридцать|сорок|пятьдесят|шестьдесят|семьдесят|восемьдесят|девяносто)"
    , regex "(перв|втор|трет|четв[её]рт|пят|шест|седьм|восьм|девят)(ь(его|ему|ей|ем|им|их|и|е)|ого|ому|ый|ой|ий|ая|ое|ья|ом|ые|ым|ых)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
         dozen <- HashMap.lookup (Text.toLower m1) cardinalsMap
         unit <- HashMap.lookup (Text.toLower m2) ordinalsFirstthMap
         Just . ordinal $ dozen + unit
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)-?((ы|о|и|а|е|ь)?(ее|й|я|е|го|му?))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }



ruleHundredsOrdinal :: Rule
ruleHundredsOrdinal = Rule
  { name = "ordinal 100..999"
  , pattern =
    [ regex "(сто|двести|триста|четыреста|пятьсот|шестьсот|семьсот|восемьсот|девятьсот)"
    , Predicate $ and . sequence [not . isMultipliable]
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token Ordinal OrdinalData{TOrdinal.value = val2}:
       _) -> do
         dozen <- HashMap.lookup (Text.toLower m1) cardinalsMap
        --  unit <- HashMap.lookup (Text.toLower m2) ordinalsFirstthMap
         Just . ordinal $ dozen + val2
      _ -> Nothing
  }

-- ruleOrdinalSum :: Rule
-- ruleOrdinalSum = Rule
--   { name = "intersect 2 numbers"
--   , pattern =
--     [ Predicate $ and . sequence [hasGrain, isPositive]
--     , Predicate $ and . sequence [not . isMultipliable]
--     ]
--   , prod = \tokens -> case tokens of
--       (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
--        Token Ordinal OrdinalData{TOrdinal.value = val2}:
--        _) | (10 ** fromIntegral g) > fromIntegral val2 -> double $ val1 + fromIntegral val2
--       _ -> Nothing
--   }

ruleOrdinalSum :: Rule
ruleOrdinalSum = Rule
  { name = "intersect 2 numbers"
  , pattern =
    [ Predicate $ and . sequence [hasGrain, isPositive]
    , Predicate $ and . sequence [not . isMultipliable]
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       Token Ordinal OrdinalData{TOrdinal.value = val2}:_) 
       | (10 ** fromIntegral g) > fromIntegral val2 -> Just $ ordinal $ round val1 + val2
      _ -> Nothing
  }

additionalNumMap :: HashMap Text.Text Int
additionalNumMap = HashMap.fromList
  [ ( "одно", 1 )
  , ( "двух", 2 )
  , ( "трех", 3 )
  , ( "четырех", 4 )
  , ( "пяти", 5 )
  , ( "шести", 6 )
  , ( "семи", 7 )
  , ( "восьми", 8 )
  , ( "девяти", 9 )
  ]

additionalUnitMap :: HashMap Text.Text Int
additionalUnitMap = HashMap.fromList
  [ ( "тысячн", 1000)
  , ( "миллионн",  1000000)
  , ( "миллиардн", 1000000000)
  ]

ruleNumeralOrdinal :: Rule
ruleNumeralOrdinal = Rule
  { name = "numeral ordinal (двухтысячный, трехмиллионный, пятимиллиардный)"
  , pattern =
    [ regex "(одно|двух|трех|четырех|пяти|шести|семи|восьми|девяти)?(тысячн|миллионн|миллиардн)(ый|ого|ом|ым|ому)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (numeral:unit:_)):_) -> do
         val <- HashMap.lookup (Text.toLower numeral) additionalNumMap
         mult <- HashMap.lookup (Text.toLower unit) additionalUnitMap
         Just . ordinal $ val * mult
      _ -> Nothing
  }


rules :: [Rule]
rules =
  [ ruleOrdinal
  , ruleOrdinalDigits
  , ruleOrdinalsFirstth
  , ruleHundredsOrdinal
  , ruleOrdinalSum
  , ruleNumeralOrdinal
  ]
