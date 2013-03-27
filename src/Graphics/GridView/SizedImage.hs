module Graphics.GridView.SizedImage (SizedImage, scaleTo, load) where

import Control.Applicative
import Control.Lens (both, (&), (%~))
import Data.Monoid (Any)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators ((%%))
import qualified Codec.Image.STB as Image
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw

data SizedImage = SizedImage
  { siSize :: Vector2 Draw.R
  , siUnscaledImage :: Draw.Image Any
  }

scaleTo :: Draw.R -> SizedImage -> Draw.Image Any
scaleTo maxSize SizedImage { siSize = size, siUnscaledImage = img } =
  Vector2.uncurry Draw.scale ((factor *) <$> size) %% img
  where
    factor = maxSize / Vector2.uncurry max size

-- siImage :: SizedImage -> Draw.Image Any
-- siImage SizedImage { siSize = (width, height), siUnscaledImage = img } =
--   Draw.scale width height %% img

load :: FilePath -> IO SizedImage
load filePath = do
  putStrLn "Loading image"
  bmp <- either fail return =<< Image.loadImage filePath
  putStrLn "Done loading image"
  sprite <- Draw.spriteBitmap bmp
  let size = uncurry Vector2 $ Draw.spriteResolution sprite & both %~ fromIntegral
  return SizedImage
    { siSize = size
    , siUnscaledImage = Draw.scale 0.5 0.5 %% Draw.translate (1, 1) %% Draw.sprite sprite
    }
