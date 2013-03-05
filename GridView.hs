{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE ViewPatterns #-}
--import qualified System.Directory as Dir
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.IORef
import Data.Monoid
import Graphics.DrawingCombinators ((%%))
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Control.Exception as E
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified System.Environment as Env

assert :: Monad m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

openWindow :: GLFW.DisplayOptions -> IO ()
openWindow options = GLFW.openWindow options >>= assert "Open window failed"

initializeGLFW :: IO ()
initializeGLFW = GLFW.initialize >>= assert "initialize failed"

withGLFWWindow :: GLFW.DisplayOptions -> IO a -> IO a
withGLFWWindow options = E.bracket_ (initializeGLFW >> openWindow options) GLFW.terminate

data SizedImage = SizedImage
  { siSize :: Draw.R2
  , siUnscaledImage :: Draw.Image Any
  }

-- siImage :: SizedImage -> Draw.Image Any
-- siImage SizedImage { siSize = (width, height), siUnscaledImage = img } =
--   Draw.scale width height %% img

debug :: String -> IO ()
debug _msg = return ()

both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)

loadSizedImage :: FilePath -> IO SizedImage
loadSizedImage filePath = do
  sprite <- Draw.openSprite filePath
  let size = both fromIntegral $ Draw.spriteResolution sprite
  return SizedImage
    { siSize = size
    , siUnscaledImage = Draw.scale 0.5 0.5 %% Draw.translate (1, 1) %% Draw.sprite sprite
    }

mainLoop :: ((Int, Int) -> IO (Draw.Image a)) -> IO b
mainLoop mkImage =
  forever $ do
    winSize@(winWidth, winHeight) <- GLFW.getWindowDimensions
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral winWidth) (fromIntegral winHeight))
    GL.clearColor $= GL.Color4 0 0 0 0
    img <- mkImage winSize
    Draw.clearRender $
      Draw.translate (-1, -1) %%
      Draw.scale (2/fromIntegral winWidth) (2/fromIntegral winHeight) %%
      img
    GLFW.pollEvents
    GLFW.swapBuffers

scaleTo :: Draw.R -> SizedImage -> Draw.Image Any
scaleTo maxSize SizedImage { siSize = (w, h), siUnscaledImage = img } =
  Draw.scale (w*factor) (h*factor) %% img
  where
    factor = maxSize / max w h

newDefault :: PrimMonad m => Int -> a -> m (MV.MVector (PrimState m) a)
newDefault l x = do
  v <- MV.new l
  forM_ [0..l-1] $ \i -> MV.write v i x
  return v

ceilDiv :: Integral a => a -> a -> a
ceilDiv x y = (x + y - 1) `div` y

align :: Integral a => a -> a -> a
align x boundary = (x `ceilDiv` boundary) * boundary

main :: IO ()
main = do
  args <- Env.getArgs
  let imgCount = length args
      fileNameAt = (V.fromList args V.!)
  imgCache <- newDefault imgCount Nothing
  withGLFWWindow GLFW.defaultDisplayOptions $ do
    GLFW.setWindowCloseCallback $ fail "Window closed"
    movementRef <- newIORef 0 :: IO (IORef Int)
    posRef <- newIORef (0, 0) :: IO (IORef (Int, Int))
    let keyPressed GLFW.KeyLeft = move (-6)
        keyPressed GLFW.KeyRight = move 6
        keyPressed _ = return $ return ()
        move delta  True  = writeIORef movementRef delta
        move _delta False = writeIORef movementRef 0
    GLFW.setKeyCallback keyPressed
    let
      gridItemSize :: Num a => a
      gridItemSize = 400
      updatePos winWidth xCount yCount movement (posIndex, posDelta)
        | newPosIndex < 0 = (0, 0)
        | newPosIndex >= rightMost && m+winWidth >= gridItemSize = (posIndex, posDelta)
        | otherwise = (newPosIndex, m)
        where
          newPosIndex = posIndex + (d * yCount)
          columns = imgCount `ceilDiv` yCount
          rightMost = (1 + columns - (xCount-1)) `align` yCount
          (d, m) = (posDelta + movement) `divMod` gridItemSize
    mainLoop $ \(winWidth, winHeight) -> do
      movement <- readIORef movementRef
      (posIndex, posDelta) <- readIORef posRef
      let
        yCount = winHeight `div` gridItemSize
        xCount = (posDelta + winWidth) `ceilDiv` gridItemSize
      modifyIORef' posRef $ updatePos winWidth xCount yCount movement
      let
        gridPositions =
          map (first (subtract posDelta) . both ((gridItemSize*) . fromIntegral)) $
          (,) <$> [0::Int ..] <*> [0..yCount-1]
        startOfRange = posIndex
        endOfRange = min (posIndex + (xCount*yCount) - 1) (imgCount-1)
        invisibleIndices =
          -- Left:
          [0..startOfRange-1] ++
          -- Right:
          [endOfRange+1 .. imgCount-1]
      forM_ invisibleIndices $ \i -> do
        maybe (return ()) (const . debug $ "Deleting image: " ++ show i) =<<
          MV.read imgCache i
        MV.write imgCache i Nothing

      imgs <- forM [startOfRange..endOfRange] $ \index -> do
        mImg <- MV.read imgCache index
        case mImg of
          Nothing -> do
            sizedImg <- loadSizedImage $ fileNameAt index
            debug $ "Loading image: " ++ show index
            MV.write imgCache index $ Just sizedImg
            return sizedImg
          Just sizedImg -> return sizedImg

      let place imgPos img = Draw.translate (both fromIntegral imgPos) %% scaleTo gridItemSize img
      return . mconcat $ zipWith place gridPositions imgs
