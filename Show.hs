import Control.Monad
import Data.Bitmap (Bitmap)
import Data.Word (Word8)
import qualified Codec.Image.STB as Image
import qualified Data.Bitmap.Pure.File as BitmapFile
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.GLFW as GLFW
import qualified System.Environment as Env

main = do
  [filePath] <- Env.getArgs
  True <- GLFW.initialize
  True <- GLFW.openWindow GLFW.defaultDisplayOptions
  GLFW.setWindowCloseCallback $ fail "Window closed"
  bmp <- BitmapFile.readBitmap filePath :: IO (Bitmap Word8)
  sprite <- Draw.spriteBitmap bmp
  forever $ do
    Draw.clearRender $ Draw.sprite sprite
    GLFW.pollEvents
    GLFW.swapBuffers
