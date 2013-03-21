{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
--import qualified System.Directory as Dir
import Control.Applicative
import Control.Lens (_1, (^.), (%~), (.~), (&), mapped, makeLenses)
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

gridItemSize :: Num a => a
gridItemSize = 400

argsArray :: IO (Int, Int -> String)
argsArray = do
  args <- Env.getArgs
  return (length args, (V.fromList args V.!))

data Scroller = Scroller
  { _sImageIndex :: Int
  , _sPixelOffset :: Int
  , _sMovementDelta :: Int
  } deriving (Show)
makeLenses ''Scroller

emptyScroller :: Scroller
emptyScroller = Scroller 0 0 0

keyPressed :: GLFW.Key -> Bool -> Scroller -> Scroller
keyPressed _ False = sMovementDelta .~ 0
keyPressed GLFW.KeyLeft True = sMovementDelta .~ (-10)
keyPressed GLFW.KeyRight True = sMovementDelta .~ 10
keyPressed _ _ = id

iteration :: Int -> Vector2 Int -> Scroller -> (Scroller, Vector2 Int)
iteration imgCount (Vector2 winWidth winHeight) s =
  ( s & sImageIndex .~ resultImageIndex
      & sPixelOffset .~ resultPixelOffset
  , Vector2 xCount yCount
  )
  where
    imageIndex = s ^. sImageIndex
    pixelOffset = s ^. sPixelOffset
    movement = (s ^. sMovementDelta)
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

run :: Int -> IndexedCache SizedImage -> IO b
run imgCount imgCache = do
  scrollerRef <- newIORef emptyScroller
  GLFW.setKeyCallback $ keyPressed & mapped . mapped %~ modifyIORef' scrollerRef
  mainLoop $ \winSize -> do
    Vector2 xCount yCount <-
      atomicModifyIORef' scrollerRef $ iteration imgCount winSize
    scroller <- readIORef scrollerRef

    let
      gridPositions =
        map ((_1 %~ subtract (scroller ^. sPixelOffset)) . fmap ((gridItemSize*) . fromIntegral)) $
        Vector2 <$> [0::Int ..] <*> [0..yCount-1]
      startOfRange = scroller ^. sImageIndex
      endOfRange = min (startOfRange + (xCount*yCount)) imgCount
      invisibleIndices =
        -- Left:
        [0..startOfRange-1] ++
        -- Right:
        [endOfRange .. imgCount-1]
    forM_ invisibleIndices $ \i -> do
      IndexedCache.delete imgCache i

    imgs <- forM [startOfRange..endOfRange-1] $ IndexedCache.get imgCache

    let
      toTuple = Vector2.uncurry (,)
      place imgPos img =
        (Draw.translate . toTuple) (fromIntegral <$> imgPos) %%
        SizedImage.scaleTo gridItemSize img
    return . mconcat . reverse $ zipWith place gridPositions imgs

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  (imgCount, fileNameAt) <- argsArray
  imgCache <- IndexedCache.new imgCount (SizedImage.load . fileNameAt)
  GLFWUtils.withGLFWWindow GLFW.defaultDisplayOptions $ do
    GLFW.setWindowCloseCallback $ fail "Window closed"
    run imgCount imgCache
