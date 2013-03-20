module GLFWUtils where

import Control.Monad
import qualified Control.Exception as E
import qualified Graphics.UI.GLFW as GLFW

assert :: Monad m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

openWindow :: GLFW.DisplayOptions -> IO ()
openWindow options = GLFW.openWindow options >>= assert "Open window failed"

initializeGLFW :: IO ()
initializeGLFW = GLFW.initialize >>= assert "initialize failed"

withGLFWWindow :: GLFW.DisplayOptions -> IO a -> IO a
withGLFWWindow options = E.bracket_ (initializeGLFW >> openWindow options) GLFW.terminate
