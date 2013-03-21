{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE ViewPatterns #-}
--import qualified System.Directory as Dir
import Control.Applicative
import Control.Lens (_1, (%~))
import Control.Monad
import Data.IORef
import Data.Monoid
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators ((%%))
import Graphics.Rendering.OpenGL.GL (($=))
import IndexedCache (IndexedCache)
import SizedImage (SizedImage)
import qualified Data.Vector as V
import qualified Data.Vector.Vector2 as Vector2
import qualified GLFWUtils as GLFWUtils
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified IndexedCache as IndexedCache
import qualified SizedImage as SizedImage
import qualified System.Environment as Env
import qualified System.IO as IO

getWinSize :: IO (Vector2 Int)
getWinSize = uncurry Vector2 <$> GLFW.getWindowDimensions

mainLoop :: (Vector2 Int -> IO (Draw.Image a)) -> IO b
mainLoop mkImage =
  forever $ do
    winSize <- getWinSize
    GL.viewport $= (GL.Position 0 0, Vector2.uncurry GL.Size $ fromIntegral <$> winSize)
    GL.clearColor $= GL.Color4 0 0 0 0
    img <- mkImage winSize
    Draw.clearRender $
      Draw.translate (-1, -1) %%
      Vector2.uncurry Draw.scale (2 / (fromIntegral <$> winSize)) %%
      img
    GLFW.pollEvents
    GLFW.swapBuffers

ceilDiv :: Integral a => a -> a -> a
ceilDiv x y = (x + y - 1) `div` y

align :: Integral a => a -> a -> a
align x boundary = (x `ceilDiv` boundary) * boundary

updatePos :: Integral a => a -> a -> a -> (a, a) -> a -> (a, a) -> (a, a)
updatePos imgCount gridItemSize winWidth (xCount, yCount) movement (posIndex, posDelta)
  | newPosIndex < 0 = (0, 0)
  | newPosIndex >= rightMost && m+winWidth >= gridItemSize = (posIndex, posDelta)
  | otherwise = (newPosIndex, m)
  where
    newPosIndex = posIndex + (d * yCount)
    columns = imgCount `ceilDiv` yCount
    rightMost = (1 + columns - (xCount-1)) `align` yCount
    (d, m) = (posDelta + movement) `divMod` gridItemSize

argsArray :: IO (Int, Int -> String)
argsArray = do
  args <- Env.getArgs
  return (length args, (V.fromList args V.!))

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  (imgCount, fileNameAt) <- argsArray
  imgCache <- IndexedCache.new imgCount (SizedImage.load . fileNameAt)
  GLFWUtils.withGLFWWindow GLFW.defaultDisplayOptions $ do
    GLFW.setWindowCloseCallback $ fail "Window closed"
    run imgCount imgCache

run :: Int -> IndexedCache SizedImage -> IO b
run imgCount imgCache = do
  movementRef <- newIORef 0
  posRef <- newIORef (0, 0)
  let keyPressed GLFW.KeyLeft = move (-10)
      keyPressed GLFW.KeyRight = move 10
      keyPressed _ = return $ return ()
      move delta  True  = writeIORef movementRef delta
      move _delta False = writeIORef movementRef 0
  GLFW.setKeyCallback keyPressed
  let
    gridItemSize :: Num a => a
    gridItemSize = 400
  mainLoop $ \(Vector2 winWidth winHeight) -> do
    movement <- readIORef movementRef
    (posIndex, posDelta) <- readIORef posRef
    let
      yCount = winHeight `div` gridItemSize
      xCount = (posDelta + winWidth) `ceilDiv` gridItemSize
    modifyIORef' posRef $ updatePos imgCount gridItemSize winWidth (xCount, yCount) movement

    let
      gridPositions =
        map ((_1 %~ subtract posDelta) . fmap ((gridItemSize*) . fromIntegral)) $
        Vector2 <$> [0::Int ..] <*> [0..yCount-1]
      startOfRange = posIndex
      endOfRange = min (posIndex + (xCount*yCount) - 1) (imgCount-1)
      invisibleIndices =
        -- Left:
        [0..startOfRange-1] ++
        -- Right:
        [endOfRange+1 .. imgCount-1]
    forM_ invisibleIndices $ IndexedCache.delete imgCache

    imgs <- forM [startOfRange..endOfRange] $ IndexedCache.get imgCache

    let
      toTuple = Vector2.uncurry (,)
      place imgPos img =
        (Draw.translate . toTuple) (fromIntegral <$> imgPos) %%
        SizedImage.scaleTo gridItemSize img
    return . mconcat . reverse $ zipWith place gridPositions imgs
