module Main (main) where

--import qualified System.Directory as Dir
import Control.Applicative
import Control.Lens (_1, (^.), (%~), (.~), (&), mapped)
import Control.Monad
import Data.IORef
import Data.Monoid
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators ((%%))
import Graphics.GridView.IndexedCache (IndexedCache)
import Graphics.GridView.MainLoop (mainLoop)
import Graphics.GridView.Scroller (Scroller)
import Graphics.GridView.SizedImage (SizedImage)
import qualified Data.Vector as V
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.GridView.IndexedCache as IndexedCache
import qualified Graphics.GridView.Scroller as Scroller
import qualified Graphics.GridView.SizedImage as SizedImage
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils
import qualified System.Environment as Env
import qualified System.IO as IO

gridItemSize :: Num a => a
gridItemSize = 400

argsArray :: IO (Int, Int -> String)
argsArray = do
  args <- Env.getArgs
  return (length args, (V.fromList args V.!))

keyPressed :: GLFW.Key -> Bool -> Scroller -> Scroller
keyPressed _ False = Scroller.sMovementDelta .~ 0
keyPressed GLFW.KeyLeft True = Scroller.sMovementDelta .~ (-10)
keyPressed GLFW.KeyRight True = Scroller.sMovementDelta .~ 10
keyPressed _ _ = id

run :: Int -> IndexedCache SizedImage -> IO b
run imgCount imgCache = do
  scrollerRef <- newIORef Scroller.empty
  GLFW.setKeyCallback $ keyPressed & mapped . mapped %~ modifyIORef' scrollerRef
  mainLoop $ \winSize -> do
    Vector2 xCount yCount <-
      atomicModifyIORef' scrollerRef $ Scroller.iteration gridItemSize imgCount winSize
    scroller <- readIORef scrollerRef

    let
      gridPositions =
        map
        ((_1 %~ subtract (scroller ^. Scroller.sPixelOffset)) .
         fmap ((gridItemSize*) . fromIntegral)) $
        Vector2 <$> [0::Int ..] <*> [0..yCount-1]
      startOfRange = scroller ^. Scroller.sImageIndex
      endOfRange = min (startOfRange + (xCount*yCount)) imgCount
      invisibleIndices =
        -- Left:
        [0..startOfRange-1] ++
        -- Right:
        [endOfRange .. imgCount-1]
    forM_ invisibleIndices $ \i ->
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
