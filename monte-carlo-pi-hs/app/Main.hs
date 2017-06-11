module Main where

import System.Random
import GHC.Float (double2Float)

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Codec.Picture.Types (Image(..))
import Graphics.Text.TrueType( loadFontFile, Font(..) )
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import MC.Pi (approximatePi)
import MC.Internal.Circle as MCI

numSamples :: Int
numSamples = 1000000

imageRadius :: Num a => a
imageRadius = 1024

samples :: StdGen -> [MCI.Point]
samples = take numSamples . randoms

transformPoint :: MCI.Point -> [Primitive]
transformPoint (MCI.Point x y) = circle (V2 (double2Float ((x + 1) * imageRadius))
                                          (double2Float ((y + 1) * imageRadius))) 1

inPoints :: [MCI.Point] -> [[Primitive]]
inPoints points = fmap transformPoint inPts
  where inPts = filter (not . MCI.isInUnitCircle) points

outPoints :: [MCI.Point] -> [[Primitive]]
outPoints points = fmap transformPoint outPts
  where outPts = filter MCI.isInUnitCircle points

main :: IO ()
main = do
  gen <- getStdGen
  fontResult <- loadFontFile "/usr/share/fonts/noto/NotoMono-Regular.ttf"
  case fontResult of
        Left err -> print $ "ERROR: " ++ err
        Right font -> writePng "/tmp/pi.png" $ createPNG gen font

createPNG :: StdGen -> Font -> Image PixelRGBA8
createPNG gen font = renderDrawing (2 * imageRadius) (2 * imageRadius) white $ do
  withTexture (uniformTexture blue) $ mapM_ fill $ inPoints points
  withTexture (uniformTexture red) $ mapM_ fill $ outPoints points
  withTexture (uniformTexture black) $ mapM_ (stroke 4 JoinRound (CapRound, CapRound)) [circle (V2 imageRadius imageRadius) imageRadius,
                                                                                         line (V2 0 imageRadius) (V2 (2 * imageRadius) imageRadius),
                                                                                         line (V2 imageRadius 0) (V2 imageRadius (2 * imageRadius))]
  withTexture (uniformTexture black) $ printTextAt font (PointSize 72) (V2 (imageRadius + 64) (imageRadius - 64)) (show pi)
  where white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        blue = PixelRGBA8 255 0 0 255
        red = PixelRGBA8 0 0 255 255
        points = samples gen
        pi = approximatePi points
