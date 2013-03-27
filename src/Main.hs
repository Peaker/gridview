module Main (main) where

import Control.Applicative
import Control.Lens (_1, (^.), (%~), (.~), (&), mapped)
import Control.Monad
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators ((%%))
import Graphics.GridView.CellArray (CellArray)
import Graphics.GridView.MainLoop (mainLoop)
import Graphics.GridView.Scroller (Scroller)
import Graphics.GridView.SizedImage (SizedImage)
import Paths_gridview (getDataFileName)
import qualified Codec.Image.STB as Image
import qualified Control.Exception as E
import qualified Data.Vector as V
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.GridView.CellArray as CellArray
import qualified Graphics.GridView.Scroller as Scroller
import qualified Graphics.GridView.SizedImage as SizedImage
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils
import qualified System.Directory as Dir
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

run :: Draw.Font -> Int -> CellArray SizedImage -> IO b
run font imgCount imgCache = do
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
      CellArray.delete imgCache i

    forM_ [startOfRange..endOfRange-1] $ CellArray.startComputing imgCache

    let loading = SizedImage.fromText font "Loading"

    imgs <-
      forM [startOfRange..endOfRange-1] $ \i ->
      fmap (fromMaybe loading) $
      CellArray.get imgCache i

    let
      toTuple = Vector2.uncurry (,)
      place imgPos img =
        (Draw.translate . toTuple) (fromIntegral <$> imgPos) %%
        SizedImage.scaleTo gridItemSize img
    return . mconcat . reverse $ zipWith place gridPositions imgs

getFont :: FilePath -> IO Draw.Font
getFont fileName = do
  e <- Dir.doesFileExist fileName
  unless e . fail $ fileName ++ " does not exist"
  Draw.openFont fileName

main :: IO ()
main = do
  font <-
    (getFont =<< getDataFileName "fonts/DejaVuSans.ttf")
    `E.catch` \(E.SomeException _) -> getFont "fonts/DejaVuSans.ttf"
  IO.hSetBuffering IO.stdout IO.LineBuffering
  (imgCount, fileNameAt) <- argsArray
  let
    loadIndex i =
      SizedImage.load =<< either fail return =<<
      Image.loadImage (fileNameAt i)
  imgCache <- CellArray.new imgCount loadIndex
  GLFWUtils.withGLFWWindow GLFW.defaultDisplayOptions $ do
    GLFW.setWindowCloseCallback $ fail "Window closed"
    run font imgCount imgCache
