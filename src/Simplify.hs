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

test4 :: Calculation
test4 = prove customLaws "cmap (cup . (one * g)) . cpp = cup . (id * cmap g)"
  where
    customLaws = [ "cupの定義: cup = cmap unify . cpp"
                 -- , "cmap-cpp則: cmap (cpp . (one * f)) . cpp = cpp . (id * cmap f)"
                 , "cmap-cpp則-逆: cpp . (id * cmap f) = cmap (cpp . (one * f)) . cpp"
                 , "cmap-cmap則: cmap f . cmap g = cmap (cmap f . g)"
                 -- , "cmap-cmap則-逆: cmap (cmap f . g) = cmap f . cmap g"
                 ]


test5 :: Calculation
test5 = prove customLaws "xmatchesA . (id * nil) = one . fst"
  where
    customLaws :: [String]
    customLaws = [ "matchの定義: match = cmap matchesA . alignments"
                 , "matchesAの定義: matchesA = combine . map matchA"
                 , "xmatchの定義: xmatch = cup . (one * match)"
                 , "xmatchesAの定義: xmatchesA = cup . (one * matchesA)"
                 , "xmatchAの定義: xmatchA = cup . (one * matchA)"
                 , "combineの定義: combine = cmap unifyAll . cp"
                 
                 , "cmap-map則: cmap f . map g = cmap (f . g)"
                 , "cmap-concat則: cmap f . concat = cmap (cmap f)"
                 , "cmap-nil則: cmap f . nil = nil"
                 , "cmap-one則: cmap f . one = f"
                 , "cmap-cup則: cmap (cup . (one * g)) . cpp = cup . (id * cmap g)"
                 , "cmap-cmap則: cmap f . cmap g = cmap (cmap f . g)"
                 , "cmap-cpp則: cmap (cpp . (one * f)) . cpp = cpp . (id * cmap f)"

                 , "map-nil則: map f . nil = nil"
                 , "map-one則: map f . one = one . f"
                 , "map-cons則: map f . cons = cons . (f * map f)"
                 , "map-concat則: map f . concat = concat . map (map f)"

                 , "cupの定義: cup = cmap unify . cpp"
                 , "cupの結合性: cup . (id * cup) = cup . (cup * id) . assocl"
                 , "cupの単位元: cup . (f * (one . nil)) = f . fst"
                 , "cupの単位元: cup . ((one . nil) * g) = g . snd"
                 , "assocl則: assocl . (f * (g * h)) = ((f * g) * h) . assocl"

                 , "(*)の双ファンクタ則: (f * g) . (h * k) = (f . h) * (g . k)"
                 , "(*)の双ファンクタ則: (id * id) = id"
                 , "cpの定義: cp . nil = one . nil"
                 , "cpの定義: cp . cons = map cons . cpp . (id * cp)"
                 , "unifyAllの定義: unifyAll . nil = one . nil"
                 , "unifyAllの定義: unifiAll . cons = cup . (one * unifiAll)"
                 , "unify-nil則: unify . (id * nil) = one . fst"

                 , "mapのファンクタ則: map f . map g = map (f . g)"
                 , "mapのファンクタ則: map id = id"
                 ]
