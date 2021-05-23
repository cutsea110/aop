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

test :: Calculation
test = prove customLaws "filter (all nodups . boxs) . expand . pruneBy boxs = filter (all nodups . boxs) . expand"
  where

    customLaws :: [String]
    customLaws = [ "pruneByの定義: pruneBy f = f . map pruneRow . f"
                 , "expand-boxs則: expand . boxs = map boxs . expand"
                 , "boxsによるfilter則: filter (p . boxs) = map boxs . filter p . map boxs"
                 , "boxsの対合性: boxs . boxs = id"
                 , "mapのファンクタ則: map f . map g = map (f . g)"
                 , "mapのファンクタ則: map id = id"
                 , "expandの定義: expand = cp . map cp"
                 , "filter-cp則: filter (all p) . cp = cp . map (filter p)"
                 , "pruneRow則: filter nodups . cp . pruneRow = filter nodups . cp"
                 , "expand-boxs則: expand . boxs = map boxs . expand"
                 , "hack: map boxs . cp . map cp = cp . map cp . boxs"
                 ]

-- TFWH p312 12.8 運算器のテスト
test2 :: Calculation
test2 = prove customLaws "xmatch = cmap xmatchesA . cpp . (one * alignments)"
  where
    customLaws :: [String]
    customLaws = [ "matchの定義: match = cmap matchesA . alignments"
                 , "xmatchの定義: xmatch = cup . (one * match)"
                 , "xmatchesAの定義: xmatchesA = cup . (one * matchesA)"
                 , "(*)の双ファンクタ則: (f * g) . (h * k) = (f . h) * (g . k)"
                 , "cmap-cup則: cmap (cup . (one * g)) . cpp = cup . (id * cmap g)"
                 ]

test3 :: Calculation
test3 = prove customLaws "xmatch s = cmap (xmatchesA s) . alignments"
  where
    customLaws = [ "matchの定義: match = cmap matchesA . alignments"
                 , "xmatchの定義: xmatch s = cmap (unify s) . match"
                 , "xmatchesAの定義: xmatchesA s = cmap (unify s) . matchesA"
                 , "cmap-cmap則: cmap f . cmap g = cmap (cmap f . g)"
                 ]

testX :: Calculation
testX = prove customLaws "xmatchesA . (id * nil) = one . fst"
  where
    customLaws :: [String]
    customLaws = [ "matchの定義: match = cmap matchesA . alignments"
                 , "matchesAの定義: matchesA = combine . map matchA"
                 , "xmatchの定義: xmatch = cup . (one * match)"
                 , "xmatchesAの定義: "
                 ]
