module Graphics.GridView.SizedImage
  ( SizedImage
  , getSize
  , scaleTo
  , fromBitmap
  , fromImage, fromText
  ) where

import Control.Applicative
import Control.Lens (both, (&), (%~))
import Control.Monad (void)
import Data.Bitmap (Bitmap)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators ((%%))
import qualified Data.Bitmap as Bitmap
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw

data SizedImage = SizedImage
  { siSize :: Vector2 Draw.R
  , siUnscaledImage :: Draw.Image ()
  }

getSize :: SizedImage -> Vector2 Draw.R
getSize = siSize

scaleTo :: Draw.R -> SizedImage -> Draw.Image ()
scaleTo maxSize SizedImage { siSize = size, siUnscaledImage = img } =
  Vector2.uncurry Draw.scale ((factor *) <$> size) %% img
  where
    factor = maxSize / Vector2.uncurry max size

-- siImage :: SizedImage -> Draw.Image Any
-- siImage SizedImage { siSize = (width, height), siUnscaledImage = img } =
--   Draw.scale width height %% img

fromImage :: Vector2 Draw.R -> Draw.Image a -> SizedImage
fromImage size image = SizedImage
  { siSize = size
  , siUnscaledImage =
    Draw.scale 0.5 0.5 %% Draw.translate (1, 1) %%
    void image
  }

fromText :: Draw.Font -> String -> SizedImage
fromText font text =
  fromImage (Vector2 (Draw.textWidth font text) 2) $
  Draw.text font text

fromBitmap :: Bitmap.PixelComponent t => Bitmap t -> IO SizedImage
fromBitmap bmp = do
  sprite <- Draw.spriteBitmap bmp
  let size = uncurry Vector2 $ Draw.spriteResolution sprite & both %~ fromIntegral
  return . fromImage size $ Draw.sprite sprite
