import Data.List (intersect)

import Test.Hspec

import Img
import Shape

main :: IO ()
main =
  hspec $ do
    describe "Polgyons" $ do
      describe "Rectangle" $ do
        it "Yields correct edges" $ do
          (length $
           intersect
             (edges (Rect {topLeft = Point {x = 0, y = 0}, bottomRight = Point {x = 10, y = 10}}))
             [ Edge {start = Point {x = 0, y = 10}, end = Point {x = 0, y = 0}}
             , Edge {start = Point {x = 0, y = 0}, end = Point {x = 10, y = 0}}
             , Edge {start = Point {x = 10, y = 0}, end = Point {x = 10, y = 10}}
             , Edge {start = Point {x = 10, y = 10}, end = Point {x = 0, y = 10}}
             ]) `shouldBe`
            4
    describe "Points" $ do
      it "Produces Pixels" $ do
        shade (1, 1, 1, 0.7) (0.2, 0.4) `shouldBe` [ Pixel {pixelX = 0, pixelY = 0, pixelColor = (1, 1, 1, 0.8 * 0.6 * 0.7)}
           , Pixel {pixelX = 1, pixelY = 0, pixelColor = (1, 1, 1, 0.8 * 0.4 * 0.7)}
           , Pixel {pixelX = 0, pixelY = 1, pixelColor = (1, 1, 1, 0.2 * 0.6 * 0.7)}
           , Pixel {pixelX = 1, pixelY = 1, pixelColor = (1, 1, 1, 0.2 * 0.4 * 0.7)}
           ]
