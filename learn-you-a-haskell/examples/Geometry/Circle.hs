module Geometry.Circle
  ( circleCircumference
  , circleArea
  ) where

circleCircumference :: Double -> Double
circleCircumference r = 2 * r * pi

circleArea :: Double -> Double
circleArea r = r * pi ^ 2
