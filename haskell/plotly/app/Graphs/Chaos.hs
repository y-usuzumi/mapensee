module Graphs.Chaos where

import           Control.Monad
import           Data.Complex
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

iterations = 200
graphingIterations = 200

initialX = 0.2
minX = 0
maxX = 1

minR = 0
maxR = 4
stepR = 0.001
rangeR = range minR maxR stepR

range :: Double -> Double -> Double -> [Double]
range minVal maxVal step = [minVal,step..maxVal]

plotCoordsToCanvasCoords :: Double -> Double -> Double -> Double -> Int -> Int -> (Double, Double) -> (Int, Int)
plotCoordsToCanvasCoords minX maxX minY maxY canvasWidth canvasHeight (x, y) = let
  plotWidth = maxX - minX
  plotHeight = maxY - minY
  coeffX = fromIntegral canvasWidth / plotWidth
  coeffY = fromIntegral canvasHeight / plotHeight
  coordX = round ((x - minX) * coeffX)
  coordY = canvasHeight - round ((y - minY) * coeffY)
  in (coordX, coordY)

iterateAndTakeValuesToPlot :: Int -> Int -> Double -> Double -> [Double]
iterateAndTakeValuesToPlot iterations graphingIterations r = take graphingIterations . drop iterations . iterate iterFunc
  where
    iterFunc a = r * a * (1-a)

draw :: Int -> Int -> UI.Canvas -> UI ()
draw canvasWidth canvasHeight canvas = do
  canvas # set' UI.fillStyle (UI.solidColor (UI.RGB 0 0 0))
  forM_ [(r, x) | r <- rangeR, x <- iterateAndTakeValuesToPlot iterations graphingIterations r initialX] $ \(r, x) -> do
    let (coordX, coordY) = plotCoordsToCanvasCoords minR maxR minX maxX canvasWidth canvasHeight (r, x)
    UI.fillRect (fromIntegral coordX, fromIntegral coordY) 1 1 canvas
