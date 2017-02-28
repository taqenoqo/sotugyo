{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Coins12 where

import Data.Data
import Data.SBV
import qualified Data.Yaml as Yaml
import qualified Data.Text as Text
import Problem

data Category =
  Prerequisite |
  Ue10 | GB1 | GB2 | GB3 | GB4 |
  MajorBasicElective |
  Comprehensive1 | Comprehensive2 | PE | English |
  Free
  deriving (SymWord, Enum, Bounded, HasKind, Ord, Eq, Show, Read, Data)

instance IsCategory Category where
  prettyPrint Prerequisite       = "必修"
  prettyPrint Ue10               = "上10"
  prettyPrint GB1                = "GB1"
  prettyPrint GB2                = "GB2"
  prettyPrint GB3                = "GB3"
  prettyPrint GB4                = "GB4"
  prettyPrint MajorBasicElective = "専門基礎選択"
  prettyPrint Comprehensive1     = "総合1"
  prettyPrint Comprehensive2     = "総合2"
  prettyPrint PE                 = "体育"
  prettyPrint English            = "英語"
  prettyPrint Free               = "自由"

instance Yaml.FromJSON Category where
  parseJSON (Yaml.String "必修")         = return Prerequisite
  parseJSON (Yaml.String "上10")         = return Ue10
  parseJSON (Yaml.String "GB1")          = return GB1
  parseJSON (Yaml.String "GB2")          = return GB2
  parseJSON (Yaml.String "GB3")          = return GB3
  parseJSON (Yaml.String "GB4")          = return GB4
  parseJSON (Yaml.String "専門基礎選択") = return MajorBasicElective
  parseJSON (Yaml.String "総合1")        = return Comprehensive1
  parseJSON (Yaml.String "総合2")        = return Comprehensive2
  parseJSON (Yaml.String "体育")         = return PE
  parseJSON (Yaml.String "英語")         = return English
  parseJSON (Yaml.String "自由")         = return Free
  parseJSON (Yaml.String s) = fail $ "Invalid category (" ++ Text.unpack s ++ ")"
  parseJSON v = fail $ "Expecting String (" ++ show v ++ ")"

graduationRule :: [CreditVar Category] -> Symbolic SBool
graduationRule creditVars = do
  let totalUpCredits' = totalUpCredits creditVars
  prerequisite       <- totalUpCredits' Prerequisite
  ue10               <- totalUpCredits' Ue10
  gb1                <- totalUpCredits' GB1
  gb2                <- totalUpCredits' GB2
  gb3                <- totalUpCredits' GB3
  gb4                <- totalUpCredits' GB4
  majorBasicElective <- totalUpCredits' MajorBasicElective
  comprehensive1     <- totalUpCredits' Comprehensive1
  comprehensive2     <- totalUpCredits' Comprehensive2
  pe                 <- totalUpCredits' PE
  english            <- totalUpCredits' English
  free               <- totalUpCredits' Free
  total              <- totalUpAllCredits creditVars
  return $
    prerequisite                  .>= toSWord 47.5 &&&
    ue10                          .>= toSWord 10   &&&
    gb1                           .>= toSWord 14   &&&
    gb1                           .<= toSWord 18   &&&
    gb2                           .>= toSWord 14   &&&
    gb2                           .<= toSWord 18   &&&
    (gb3 + gb4)                   .>= toSWord 6.5  &&&
    (gb3 + gb4)                   .<= toSWord 10.5 &&&
    (ue10+ gb1 + gb2 + gb3 + gb4) .>= toSWord 48.5 &&&
    majorBasicElective            .>= toSWord 8    &&&
    comprehensive1                .>= toSWord 2    &&&
    comprehensive2                .>= toSWord 6    &&&
    pe                            .>= toSWord 3    &&&
    english                       .>= toSWord 4.5  &&&
    free                          .>= toSWord 6.5  &&&
    total                         .>= toSWord 126

replaceCredits :: [Credit Category] -> [Credit Category]
replaceCredits credits = replaceCredits' credits additionalCredits where
  replaceCredits' [] addedCredits = addedCredits
  replaceCredits' (credit:credits) addedCredits = case credit of
    Credit _ "データ構造とアルゴリズム" 3.0 _ ->
      let dsa        = Credit 0 "データ構造とアルゴリズム" 2.0 [Prerequisite] in
      let surplusDsa = Credit 0 "データ構造とアルゴリズム (余剰)" 1.0 [GB1] in
      dsa : replaceCredits' credits (surplusDsa : addedCredits)
    Credit _ "データ構造とアルゴリズム実験" 1.5 _ ->
      let dsa        = Credit 0 "データ構造とアルゴリズム実験" 1.0 [Prerequisite] in
      let surplusDsa = Credit 0 "データ構造とアルゴリズム実験 (余剰)" 0.5 [GB1] in
      dsa : replaceCredits' credits (surplusDsa : addedCredits)
    _ -> credit : replaceCredits' credits addedCredits

  additionalCredits = [
      Credit 0 "専門語学" 3.0 [Prerequisite]
    ]

