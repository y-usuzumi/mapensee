module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Complex
import           Debug.Trace
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

canvasSize = 800
iterations = 15

checkDivergence_ :: RealFloat a => Complex a -> Complex a -> Int -> Int -> Int
checkDivergence_ complex z currIteration totalIteration
  | currIteration >= totalIteration = currIteration
  | realPart (abs z) > 2 = currIteration
  | otherwise = checkDivergence_ complex (z * z + complex) (currIteration + 1) totalIteration

checkDivergence :: RealFloat a => Complex a -> Int -> Int
checkDivergence complex = checkDivergence_ complex 0 0

plotCoordsToCanvasCoords :: Double -> Double -> Double -> Double -> Int -> Int -> (Double, Double) -> (Int, Int)
plotCoordsToCanvasCoords minX maxX minY maxY canvasWidth canvasHeight (x, y) = let
  plotWidth = maxX - minX
  plotHeight = maxY - minY
  coeffX = fromIntegral canvasWidth / plotWidth
  coeffY = fromIntegral canvasHeight / plotHeight
  coordX = round ((x - minX) * coeffX)
  coordY = round ((y - minY) * coeffY)
  in (coordX, coordY)

setup :: Window -> UI ()
setup window = do
  void $ return window # set title "Canvas - Example"
  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width  canvasSize
    # set style [("border", "solid black 1px"), ("background", "#eee")]

  canvas # set' UI.fillStyle (UI.solidColor (UI.RGB 255 0 0))

  drawMandelbrot <- UI.button #+ [string "Draw Mandelbrot"]
  clear     <- UI.button #+ [string "Clear the canvas."]

  void $ getBody window #+
    [ column [element canvas]
    , element drawMandelbrot, element clear
    ]

  on UI.click drawMandelbrot $ const $ do
    forM_ [(x, y) | x <- [-2.5,-2.495..1.5], y <- [-2,-1.995..2]] $ \(x, y) -> do
      let (coordX, coordY) = plotCoordsToCanvasCoords (negate 2.5) 1.5 (negate 2) 2 canvasSize canvasSize (x, y)
      let divergence = checkDivergence (x :+ y) iterations
      when (divergence < 20) $ do
        canvas # set' UI.fillStyle (UI.solidColor (UI.RGB (255 `div` iterations * divergence) 0 0))
        UI.fillRect (fromIntegral coordX, fromIntegral coordY) 1 1 canvas

main :: IO ()
main = do
  startGUI defaultConfig setup
