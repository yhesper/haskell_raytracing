module Lib
    ( bdptApp
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Brick
import qualified Graphics.Vty as V
import Linear.V2
import Linear.V3

data Tick = Tick

-- | Named resources
type Name = ()
type Image = ()

initialState :: Image
initialState = ()

height :: Int
height = 25
width :: Int
width = 25

drawGrid :: Image -> Widget Name
drawGrid img = vBox rows
  where 
    rows =  [ hBox $ pixelsInRow r | r <- [height-1,height-2..0]]
    pixelsInRow y = [drawPixel (pixelAt (V2 x y)) | x <- [0..width-1]]
    pixelAt (V2 x y)
      | x == 0 && y == 0 = V3 255 0 0
      | otherwise = V3 0 255 0

-- Take an rgb value and draw the pixel as a widget
drawPixel :: V3 Int -> Widget Name
drawPixel (V3 r g b) = withAttr (attrName (rgbToAttrName r' g' b')) (str "██")
  where
    r' = mapToMaxColorResolution r
    g' = mapToMaxColorResolution g
    b' = mapToMaxColorResolution b

drawUI :: Image -> [Widget Name]
drawUI im = [drawGrid im]

handleEvent :: BrickEvent Name Tick -> EventM Name Image ()
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
