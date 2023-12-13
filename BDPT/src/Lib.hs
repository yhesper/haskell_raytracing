module Lib
    ( bdptApp
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (when, void, forever)
import Brick
import Brick.BChan
import qualified Graphics.Vty as V
import Linear.V2
import Linear.V3
import Scene

data Tick = Tick

-- | Named resources
data Name = ClickableImage

instance Eq Name where
  ClickableImage == ClickableImage = True
instance Ord Name where
  ClickableImage < ClickableImage = False
  ClickableImage <= ClickableImage = True

type Image = [V3 Float]
data BDPTRenderState = MkBDPTRenderState {
  _sampleIdx :: Int,
  _img :: Image,
  _scene :: Scene.Scene,
  _selectedPrimitiveIdx :: Maybe Int
}

emptyImg :: Image
emptyImg = replicate (width * height) (V3 0 0 0)

initialState :: BDPTRenderState
initialState =
  MkBDPTRenderState
  0
  emptyImg
  test2
  Nothing

height :: Int
height = 80
width :: Int
width = 80
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

drawSelectedPrimitiveIdx :: Maybe Int -> Widget Name
drawSelectedPrimitiveIdx idx = str ("Selected primitive: " ++ maybe "None" show idx)

drawSampleProgress :: Int -> Widget Name
drawSampleProgress s = str (show s ++ "/" ++ show spp ++ " spp")

drawUI :: BDPTRenderState -> [Widget Name]
drawUI bdptRS =
  [
    clickable ClickableImage (drawImage (_img bdptRS))
    <+>
    padLeft (Pad 4)
    (vBox [
      drawSampleProgress (_sampleIdx bdptRS),
      drawSelectedPrimitiveIdx (_selectedPrimitiveIdx bdptRS)
    ])
  ]

handleEvent :: BrickEvent Name Tick -> EventM Name BDPTRenderState ()
handleEvent (AppEvent Tick) = do
  (MkBDPTRenderState sampleIdx _ scene primitiveIdx) <- get
  if sampleIdx >= spp then
    return ()
  else do
    let img = Scene.render scene width height
    put $ MkBDPTRenderState (sampleIdx+1) img scene primitiveIdx
  
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
-- Reset rendering to initial
handleEvent (VtyEvent (V.EvKey V.KEnter      [])) = do
  (MkBDPTRenderState _ _ scene primitiveIdx) <- get
  put $ MkBDPTRenderState 0 emptyImg scene primitiveIdx
handleEvent (MouseUp ClickableImage _ (Location (x, y))) = do
  (MkBDPTRenderState sampleIdx img scene _) <- get
  -- Each "pixel" is made up of 2 chars, so divide x coord by 2
  let coords = (x `div` 2, height-y)
  let primitiveIdx = rayCastPrimitive test2 coords (width, height)
  put $ MkBDPTRenderState sampleIdx img scene primitiveIdx
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

-- Needed to enable clicking
enableMouseEvent :: EventM Name BDPTRenderState ()
enableMouseEvent = do
  vty <- getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.Mouse) $
    liftIO $ V.setMode output V.Mouse True

bdptApp :: IO ()
bdptApp = do
  let app =  App {
    appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = enableMouseEvent
  , appAttrMap      = const attributeMap
  }

  chan <- newBChan 1
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay $ round (167e3 :: Float) -- Render at up to 60 fps - 16.7 ms per tick

  -- Run the Brick app
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  _ <- customMain initialVty builder (Just chan) app initialState
  return ()
