module SizedImage where

import Control.Lens (both, (&), (%~))
import Data.Monoid (Any)
import Graphics.DrawingCombinators ((%%))
import qualified Codec.Image.STB as Image
import qualified Graphics.DrawingCombinators as Draw

data SizedImage = SizedImage
  { siSize :: Draw.R2
  , siUnscaledImage :: Draw.Image Any
  }

scaleTo :: Draw.R -> SizedImage -> Draw.Image Any
scaleTo maxSize SizedImage { siSize = (w, h), siUnscaledImage = img } =
  Draw.scale (w*factor) (h*factor) %% img
  where
    factor = maxSize / min w h

-- siImage :: SizedImage -> Draw.Image Any
-- siImage SizedImage { siSize = (width, height), siUnscaledImage = img } =
--   Draw.scale width height %% img

load :: FilePath -> IO SizedImage
load filePath = do
  putStrLn "Loading image"
  bmp <- either fail return =<< Image.loadImage filePath
  putStrLn "Done loading image"
  sprite <- Draw.spriteBitmap bmp
  let size = Draw.spriteResolution sprite & both %~ fromIntegral
  return SizedImage
    { siSize = size
    , siUnscaledImage = Draw.scale 0.5 0.5 %% Draw.translate (1, 1) %% Draw.sprite sprite
    }
