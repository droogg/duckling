-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.RU.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "ноль"
             , "нисколько"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "один"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "02"
             , "два"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "три"
             , "03"
             , "трех"
             ]
  , examples (NumeralValue 4)
             [ "4"
             , "четыре"
             , "04"
             ]
  , examples (NumeralValue 5)
             [ "пять"
             , "5"
             , "05"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "тридцать три"
             , "тридцати три"
             , "0033"
             ]
  , examples (NumeralValue 11)
             [ "11"
             , "одиннадцать"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "четырнадцать"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "шестнадцать"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "семнадцать"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "восемнадцать"
             ]
  , examples (NumeralValue 312)
             [ "триста двенадцать"
             , "312"
             ]
  , examples (NumeralValue 444)
             [ "четыреста сорок четыре"
             , "четыреста сорока четыре"
             , "444"
             ]
  , examples (NumeralValue 525)
             [ "пятьсот двадцать пять"
             , "пятьсот двадцати пять"
             , "525"
             ]
  , examples (NumeralValue 555)
             [ "пятьсот пятьдесят пять"
             , "пятьсот пятидесяти пять"
             ]
  , examples (NumeralValue 565)
             [ "пятьсот шестьдесят пять"
             , "пятьсот шестидесяти пять"
             ]
  , examples (NumeralValue 575)
             [ "пятьсот семьдесят пять"
             , "пятьсот семидесяти пять"
             ]
  , examples (NumeralValue 585)
             [ "пятьсот восемьдесят пять"
             , "пятьсот восьмидесяти пять"
             ]
  , examples (NumeralValue 595)
             [ "пятьсот девяносто пять"
             , "пятьсот девяноста пять"
             ]
  , examples (NumeralValue 1.5)
             [ "1.5"
             , "полторы"
             , "один с половиной"
             ]
  , examples (NumeralValue 3.5)
             [ "3.5"
             , "три с половиной"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 точка 1"
             , "один точка один"
             ]
  , examples (NumeralValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumeralValue 100000)
             [ "100000"
             , "100к"
             , "100К"
             ]
  , examples (NumeralValue 3000000)
             [ "3М"
             , "3000К"
             , "3000000"
             ]
  , examples (NumeralValue 1200000)
             [ "1200000"
             , "1.2М"
             , "1200К"
             , ".0012Г"
             ]
  , examples (NumeralValue (-1200000))
             [ "-1200000"
             , "минус 1200000"
             , "-1.2М"
             , "-1200К"
             , "-.0012Г"
             ]
  , examples (NumeralValue 1000000)
             [ "1 миллион"
             ]
  , examples (NumeralValue 2000000)
             [ "2 миллиона"
             ]
  , examples (NumeralValue 15000000)
             [ "15 миллионов"
             ]
  , examples (NumeralValue 121000000)
             [ "121 миллион"
             ]
  , examples (NumeralValue 1000)
             [ "1 тысяча"
             ]
  , examples (NumeralValue 3000)
             [ "3 тысячи"
             ]
  , examples (NumeralValue 10000)
             [ "10 тысяч"
             ]
  , examples (NumeralValue 1000000000)
             [ "1 миллиард"
             ]
  , examples (NumeralValue 10000000000)
             [ "10 миллиардов"
             ]
  , examples (NumeralValue 10000000000)
             [ "10 миллиарда"
             ]
  , examples (NumeralValue 1121)
             [ "одна тысяча сто двадцать один"
             ]
  , examples (NumeralValue 2023)
             [ "две тысячи двадцать три"
             ]
  , examples (NumeralValue 1123123)
             [ "один миллион сто двадцать три тысячи сто двадцать три"
             ]
  ]
