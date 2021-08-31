module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Complex
import           Debug.Trace
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphs.Mandelbrot as Mandelbrot
import qualified Graphs.Chaos as Chaos

canvasSize = 800


setup :: Window -> UI ()
setup window = do
  void $ return window # set title "Canvas - Example"
  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width  canvasSize
    # set style [("border", "solid black 1px"), ("background", "#eee")]

  canvas # set' UI.fillStyle (UI.solidColor (UI.RGB 255 0 0))

  btnDrawMandelbrot <- UI.button #+ [string "Draw Mandelbrot"]
  btnDrawChaos <- UI.button #+ [string "Draw Chaos"]
  clear     <- UI.button #+ [string "Clear the canvas."]

  void $ getBody window #+
    [ column [element canvas]
    , element btnDrawMandelbrot, element btnDrawChaos, element clear
    ]

  on UI.click btnDrawMandelbrot $ const $ Mandelbrot.draw canvasSize canvasSize canvas
  on UI.click btnDrawChaos $ const $ Chaos.draw canvasSize canvasSize canvas

main :: IO ()
main = do
  startGUI defaultConfig setup
