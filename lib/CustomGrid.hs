{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  CustomGrid
-- Copyright   :  (c) James Nylen
--
-- Shamelessly kanged from:
--
-- Module      :  XMonad.Layout.Grid
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A simple layout that attempts to put all windows in a square grid, but
-- for 3 windows, uses a special layout with 1 on top and 2 on bottom.
--
-----------------------------------------------------------------------------

module CustomGrid (
    -- * Usage
    -- $usage
    CustomGrid(..), arrange, defaultRatio
) where

import XMonad
import XMonad.StackSet

data CustomGrid a = CustomGrid | GridRatio Double deriving (Read, Show)

defaultRatio :: Double
defaultRatio = 16/9

instance LayoutClass CustomGrid a where
    pureLayout CustomGrid          r = pureLayout (GridRatio defaultRatio) r
    pureLayout (GridRatio d) r = arrange d r . integrate

arrange :: Double -> Rectangle -> [a] -> [(a, Rectangle)]
arrange aspectRatio (Rectangle rx ry rw rh) st =
  if nwins == 3 then
    let hw = rw `div` 2
        hh = rh `div` 2
        hx = fromIntegral hw
        hy = fromIntegral hh
    in zip st [
         Rectangle (rx     )  (ry     )  (rw     )  (hh     )
       , Rectangle (rx     )  (ry + hy)  (hw     )  (rh - hh)
       , Rectangle (rx + hx)  (ry + hy)  (rw - hw)  (rh - hh)
       ]
 else zip st rectangles
    where
    nwins = length st
    ncols = max 1 . round . sqrt $ fromIntegral nwins * fromIntegral rw / (fromIntegral rh * aspectRatio)
    mincs = max 1 $ nwins `div` ncols
    extrs = nwins - ncols * mincs
    chop :: Int -> Dimension -> [(Position, Dimension)]
    chop n m = ((0, m - k * fromIntegral (pred n)) :) . map (flip (,) k) . tail . reverse . take n . tail . iterate (subtract k') $ m'
        where
        k :: Dimension
        k = m `div` fromIntegral n
        m' = fromIntegral m
        k' :: Position
        k' = fromIntegral k
    xcoords = chop ncols rw
    ycoords = chop mincs rh
    ycoords' = chop (succ mincs) rh
    (xbase, xext) = splitAt (ncols - extrs) xcoords
    rectangles = combine ycoords xbase ++ combine ycoords' xext
        where
        combine ys xs = [Rectangle (rx + x) (ry + y) w h | (x, w) <- xs, (y, h) <- ys]
