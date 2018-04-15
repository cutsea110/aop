module Cpp where

import FixPrime

cpp (x, y) = [(a, b) | a <- x, b <- y]
cpr (a, y) = [(a, b) | b <- y]
cpl (x, b) = [(a, b) | a <- x]
