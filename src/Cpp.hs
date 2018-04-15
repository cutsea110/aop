module Cpp where

import FixPrime

cpp (x, y) = [(a, b) | a <- x, b <- y]
cpl (x, b) = [(a, b) | a <- x]
