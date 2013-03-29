{-# LANGUAGE TemplateHaskell #-}
module Graphics.GridView.Scroller(Scroller(..), empty, iteration, sImageIndex, sPixelOffset, sMovementDelta) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens (_1, (^.), (%~), (.~), (&), makeLenses)
import Data.Vector.Vector2 (Vector2(..))

data Scroller = Scroller
  { gridItemSize :: Int
  , _sImageIndex :: Int
  , _sPixelOffset :: Int
  , _sMovementDelta :: Int
  } deriving (Show)
makeLenses ''Scroller

empty :: Int -> Scroller
empty gis = Scroller gis 0 0 0

ceilDiv :: Integral a => a -> a -> a
ceilDiv x y = (x + y - 1) `div` y

align :: Integral a => a -> a -> a
align x boundary = (x `ceilDiv` boundary) * boundary

iteration :: Int -> Vector2 Int -> Scroller -> (Scroller, (Vector2 Int, [Vector2 Int]))
iteration imgCount (Vector2 winWidth winHeight) s =
  ( s & sImageIndex .~ resultImageIndex
      & sPixelOffset .~ resultPixelOffset
  , ( Vector2 xCount yCount
    , gridPositions
    )
  )
  where
    gridPositions =
      map
      ((_1 %~ subtract (s ^. sPixelOffset)) .
       fmap ((gridItemSize s *) . fromIntegral)) $
      Vector2 <$> [0::Int ..] <*> [0..yCount-1]
    imageIndex = s ^. sImageIndex
    pixelOffset = s ^. sPixelOffset
    movement = s ^. sMovementDelta
    yCount = winHeight `div` gridItemSize s
    xCount = 1 + (winWidth `ceilDiv` gridItemSize s)
    (resultImageIndex, resultPixelOffset)
      | newPosIndex < 0 = (0, 0)
      | newPosIndex >= rightMost && m+winWidth >= gridItemSize s = (imageIndex, pixelOffset)
      | otherwise = (newPosIndex, m)
      where
        newPosIndex = imageIndex + (d * yCount)
        columns = imgCount `ceilDiv` yCount
        rightMost = (1 + columns - (xCount-1)) `align` yCount
        (d, m) = (pixelOffset + movement) `divMod` gridItemSize s
