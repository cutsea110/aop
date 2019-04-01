module DrawMatrix
  ( drawWith
  , draw
  , draw'
  ) where

import Control.Monad (forM_)
import Text.Printf (printf)

type Width = Int

drawWith :: Maybe Width -> (([Int], [Int]) -> [[Int]]) -> [Int] -> [Int] -> IO ()
drawWith mw calc rs cs = do
  putHdr
  forM_ (zip rs anss) putRow
  where
    anss = calc (rs, cs)
    digits = length . show . maximum
    (dr, dr', dr'') = (digits rs, dr+1, dr'+1)
    (ds, ds') = (maybe (digits (map maximum anss)) id mw, ds+1)
    pf = printf ("%" ++ show ds' ++ "d")
    indent = putStr (replicate dr'' ' ')
    
    putHdr = do
      indent >> forM_ cs pf >> putChar '\n'
      indent >> putStr (replicate (length cs * ds') '=') >> putChar '\n'
    putRow (x, ys) = do
      printf ("%" ++ show dr' ++ "d|") x
      forM_ ys pf >> putChar '\n'

draw = drawWith Nothing
draw' = drawWith . Just
