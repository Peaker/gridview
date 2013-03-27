{-# LANGUAGE TemplateHaskell #-}
module Graphics.GridView.Scroller(Scroller(..), empty, iteration, sImageIndex, sPixelOffset, sMovementDelta) where

import Control.Lens ((^.), (.~), (&), makeLenses)
import Data.Vector.Vector2 (Vector2(..))

data Scroller = Scroller
  { _sImageIndex :: Int
  , _sPixelOffset :: Int
  , _sMovementDelta :: Int
  } deriving (Show)
makeLenses ''Scroller

empty :: Scroller
empty = Scroller 0 0 0

ceilDiv :: Integral a => a -> a -> a
ceilDiv x y = (x + y - 1) `div` y

align :: Integral a => a -> a -> a
align x boundary = (x `ceilDiv` boundary) * boundary

iteration :: Int -> Int -> Vector2 Int -> Scroller -> (Scroller, Vector2 Int)
iteration gridItemSize imgCount (Vector2 winWidth winHeight) s =
  ( s & sImageIndex .~ resultImageIndex
      & sPixelOffset .~ resultPixelOffset
  , Vector2 xCount yCount
  )
  where
    imageIndex = s ^. sImageIndex
    pixelOffset = s ^. sPixelOffset
    movement = s ^. sMovementDelta
    yCount = winHeight `div` gridItemSize
    xCount = 1 + (winWidth `ceilDiv` gridItemSize)
    (resultImageIndex, resultPixelOffset)
      | newPosIndex < 0 = (0, 0)
      | newPosIndex >= rightMost && m+winWidth >= gridItemSize = (imageIndex, pixelOffset)
      | otherwise = (newPosIndex, m)
      where
        newPosIndex = imageIndex + (d * yCount)
        columns = imgCount `ceilDiv` yCount
        rightMost = (1 + columns - (xCount-1)) `align` yCount
        (d, m) = (pixelOffset + movement) `divMod` gridItemSize
