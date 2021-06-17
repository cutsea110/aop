module Cpp where

import FixPrime

cpp (x, y) = [(a, b) | a <- x, b <- y]
cpr (a, y) = [(a, b) | b <- y]
cpl (x, b) = [(a, b) | a <- x]

cpp' (x, y) = do
  a <- x
  b <- y
  return (a, b)

cpr' (a, y) = do
  b <- y
  return (a, b)

cpl' (x, b) = do
  a <- x
  return (a, b)
