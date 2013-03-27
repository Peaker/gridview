module Graphics.GridView.MainLoop (mainLoop) where

--import qualified System.Directory as Dir
import Control.Applicative
import Control.Monad
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators ((%%))
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

mainLoop :: (Vector2 Int -> IO (Draw.Image a)) -> IO b
mainLoop mkImage =
  forever $ do
    winSize <- uncurry Vector2 <$> GLFW.getWindowDimensions
    GL.viewport $= (GL.Position 0 0, Vector2.uncurry GL.Size $ fromIntegral <$> winSize)
    GL.clearColor $= GL.Color4 0 0 0 0
    img <- mkImage winSize
    Draw.clearRender $
      Draw.translate (-1, -1) %%
      Vector2.uncurry Draw.scale (2 / (fromIntegral <$> winSize)) %%
      img
    GLFW.pollEvents
    GLFW.swapBuffers
