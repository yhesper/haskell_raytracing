module Lib
    ( bdptApp
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Brick
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Linear.V2
import Linear.V3
import Scene

data Tick = Tick

-- | Named resources
type Name = ()
type Image = [V3 Float]
data BDPTRenderState = MkBDPTRenderState {
  sampleIdx :: Int,
  img :: Image
}

initialState :: BDPTRenderState
initialState = MkBDPTRenderState 0 (Scene.render test1 width height) --(replicate (height * width) (V3 0 0 0))

height :: Int
height = 25
width :: Int
width = 25
spp :: Int
spp = 32

drawImage :: Image -> Widget Name
drawImage img = vBox rows
  where 
    rows =  [ hBox $ pixelsInRow r | r <- [height-1,height-2..0]]
    pixelsInRow y = [drawPixel (pixelAt (V2 x y)) | x <- [0..width-1]]
    pixelAt (V2 x y) = img !! (y * width + x)

-- Take an rgb value and draw the pixel as a widget
drawPixel :: V3 Float -> Widget Name
drawPixel (V3 r g b) = withAttr (attrName (rgbToAttrName r' g' b')) (str "██")
  where
    r' = mapToMaxColorResolution (round (r * 255) :: Int)
    g' = mapToMaxColorResolution (round (g * 255) :: Int)
    b' = mapToMaxColorResolution (round (b * 255) :: Int)

drawSampleProgress :: Int -> Widget Name
drawSampleProgress s = str (show s ++ "/" ++ show spp ++ " spp")

drawUI :: BDPTRenderState -> [Widget Name]
drawUI bdptRS =
  [C.center $
    padRight (Pad 4) (drawSampleProgress (sampleIdx bdptRS + 1))
    <+>
    drawImage (img bdptRS)
  ]

handleEvent :: BrickEvent Name Tick -> EventM Name BDPTRenderState ()
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent _ = return ()

-- The attribute map cannot support 255^3 color combinations, so specify
-- the maximum range of rgb values
maxColorResolution :: Int
maxColorResolution = 100

-- Functions to map between 0..255 and 0..maxColorResolution
mapTo255 :: Integral a => a -> Int
mapTo255 x = round ((fromIntegral x :: Float) * (255.0 / (fromIntegral maxColorResolution :: Float)))

mapToMaxColorResolution :: Integral a => a -> Int
mapToMaxColorResolution x = round ((fromIntegral x :: Float) * ((fromIntegral maxColorResolution :: Float) / 255.0))

rgbToAttrName :: Int -> Int -> Int -> String
rgbToAttrName r g b = show r ++ "-" ++ show g ++ "-" ++ show b

-- Generate attribute map for colors in form of r-g-b -> RGBColor r g b
colorMap :: [(AttrName, V.Attr)]
colorMap = do
  r <- [0..maxColorResolution] :: [Int]
  g <- [0..maxColorResolution] :: [Int]
  b <- [0..maxColorResolution] :: [Int]
  let attrColor = V.linearColor (mapTo255 r) (mapTo255 g) (mapTo255 b)
  return (attrName (rgbToAttrName r g b), fg attrColor)

attributeMap :: AttrMap
attributeMap = attrMap
  V.defAttr colorMap

bdptApp :: IO ()
bdptApp = do
  let app =  App {
    appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const attributeMap
  }

  -- Run the Brick app
  _ <- defaultMain app initialState
  return ()
