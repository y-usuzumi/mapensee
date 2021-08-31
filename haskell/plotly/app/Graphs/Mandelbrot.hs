module Graphs.Mandelbrot where

import           Control.Monad
import           Data.Complex
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

iterations = 100

rangeX = [-2.5, -2.495..1.5]
rangeY = [-2, -1.995..2]

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

draw :: Int -> Int -> UI.Canvas -> UI ()
draw canvasWidth canvasHeight canvas = do
  forM_ [(x, y) | x <- rangeX, y <- rangeY] $ \(x, y) -> do
    let (coordX, coordY) = plotCoordsToCanvasCoords (negate 2.5) 1.5 (negate 2) 2 canvasWidth canvasHeight (x, y)
        divergence = checkDivergence (x :+ y) iterations
        rgbVal = round $ 255 / fromIntegral iterations * fromIntegral (iterations - divergence)
    canvas # set' UI.fillStyle (UI.solidColor (UI.RGB rgbVal rgbVal rgbVal))
    UI.fillRect (fromIntegral coordX, fromIntegral coordY) 1 1 canvas
