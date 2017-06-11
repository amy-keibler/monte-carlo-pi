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

imageDiameter :: Num a => a
imageDiameter = 2 * imageRadius

samples :: StdGen -> [MCI.Point]
samples = take numSamples . randoms

transformPoint :: MCI.Point -> [Primitive]
transformPoint (MCI.Point x y) = circle (V2 (double2Float $ toImageScale x)
                                          (double2Float $ toImageScale y)) 1
  where toImageScale num = (num + 1) * imageRadius

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
        Left err -> print err
        Right font -> writePng "/tmp/pi.png" $ createPNG gen font

createPNG :: StdGen -> Font -> Image PixelRGBA8
createPNG gen font = renderDrawing imageDiameter imageDiameter white $ do
  withColor blue $ mapM_ fill $ inPoints points
  withColor red $ mapM_ fill $ outPoints points
  withColor black $ mapM_ (stroke 4 JoinRound (CapRound, CapRound)) [circle (V2 imageRadius imageRadius) imageRadius,
                                                                     line (V2 0 imageRadius) (V2 imageDiameter imageRadius),
                                                                     line (V2 imageRadius 0) (V2 imageRadius imageDiameter)]
  withColor black $ printTextAt font (PointSize 72) (V2 (imageRadius + 64) (imageRadius - 64)) $ show pi
  where white = PixelRGBA8 255 255 255 255
        black = PixelRGBA8 0 0 0 255
        blue = PixelRGBA8 255 0 0 255
        red = PixelRGBA8 0 0 255 255
        points = samples gen
        pi = approximatePi points
        withColor = withTexture . uniformTexture
