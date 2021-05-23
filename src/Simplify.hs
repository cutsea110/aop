module Simplify
  ( simplify
  , prove
  ) where

import Calculations (Calculation, calculate, paste)
import Parsing
import Laws
import Expressions

simplify :: [String] -> String -> Calculation
simplify strings string = let laws = map (parse law) strings
                              e = parse expr string
                          in calculate laws e

prove :: [String] -> String -> Calculation
prove strings string = let laws = map (parse law) strings
                           (e1, e2) = parse equation string
                       in paste (calculate laws e1) (calculate laws e2)


-- TFWH p305 12.8 運算器のテスト

customLaws :: [String]
customLaws = [ "pruneByの定義: pruneBy f = f . map pruneRow . f"
             , "expand-boxs則: expand . boxs = map boxs . expand"
             , "boxsによるfilter則: filter (p . boxs) = map boxs . filter p . map boxs"
             , "boxsの対合性: boxs . boxs = id"
             , "mapのファンクタ則: map f . map g = map (f . g)"
             -- , "mapのファンクタ則: map (f . g) . boxs = map f . map g"
             , "mapのファンクタ則: map id = id"
             , "expandの定義: expand = cp . map cp"
             , "filter-cp則: filter (all p) . cp = cp . map (filter p)"
             , "pruneRow則: filter nodups . cp . pruneRow = filter nodups . cp"
             , "expand-boxs則: expand . boxs = map boxs . expand"
             , "hack: map boxs . cp . map cp = cp . map cp . boxs"
             ]

test :: Calculation
test = prove customLaws "filter (all nodups . boxs) . expand . pruneBy boxs = filter (all nodups . boxs) . expand"
