module Lib
    ( bdptApp
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (when, void, forever)
import Brick
import qualified Brick.Widgets.Center as C
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

data ColorChannel = CCR | CCG | CCB

type Image = [V3 Float]
data BDPTRenderState = MkBDPTRenderState {
  _sampleIdx :: Int,
  _img :: Image,
  _scene :: Scene.Scene,
  _selectedPrimitiveIdx :: Maybe Int,
  _selectedColorChannel :: ColorChannel
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
  CCR

height :: Int
height = 40
width :: Int
width = 40
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
drawSampleProgress s = str ("Progress: " ++ show s ++ "/" ++ show spp ++ " spp")

drawSelectedPrimitive :: Scene -> Maybe Int -> Widget Name
drawSelectedPrimitive scene idx = vBox $ str ("Selected primitive: " ++ maybe "None" show idx) : primitiveInfo
  where
    primitiveInfo = maybe [] drawPrimitiveInfo idx
    drawPrimitiveInfo i =
      let primitive = primitives scene !! i
          (V3 r g b) = sphere_color primitive
          r' = show (round (r * 255) :: Int)
          g' = show (round (g * 255) :: Int)
          b' = show (round (b * 255) :: Int)
      in    
        [str $ "    RGB Color: " ++ r' ++ ", " ++ g' ++ ", " ++ b']

drawSelectedColorChannel :: ColorChannel -> Widget Name
drawSelectedColorChannel cc = str
  ("Selected color channel: " ++
    case cc of
      CCR -> "R"
      CCG -> "G"
      CCB -> "B"
  )

drawUI :: BDPTRenderState -> [Widget Name]
drawUI bdptRS =
  [
    clickable ClickableImage (drawImage (_img bdptRS))
    <+>
    padLeft (Pad 4)
    (vBox [
      str "Haskell Renderer",
      str " ",
      drawSampleProgress (_sampleIdx bdptRS),
      C.vCenter
      (vBox [
        str "Scene Editing:",
        str " ",
        drawSelectedColorChannel (_selectedColorChannel bdptRS),
        drawSelectedPrimitive (_scene bdptRS) (_selectedPrimitiveIdx bdptRS)
      ]),
      C.vCenter
      (vBox [
        str "Controls:",
        str " ",
        str "Esc:               Quit",
        str "Left Click:        Select object to edit",
        str "R, G, B:           Select color channel to edit",
        str "Up/Down Arrow:     Increase/Decrease color channel"
      ])
    ])
  ]

updateChannelColor :: ColorChannel -> EventM Name BDPTRenderState ()
updateChannelColor colorChannel = do
  (MkBDPTRenderState sampleIdx img scene primitiveIdx _) <- get
  put $ MkBDPTRenderState sampleIdx img scene primitiveIdx colorChannel

updateColorChannel :: Float -> Int -> Float
updateColorChannel c delta = clamp 0 255 (c*255 + (fromIntegral delta :: Float)) / 255

updateColor :: ColorChannel -> V3 Float -> Int -> V3 Float
updateColor CCR (V3 r g b) delta = V3 (updateColorChannel r delta) g b
updateColor CCG (V3 r g b) delta = V3 r (updateColorChannel g delta) b
updateColor CCB (V3 r g b) delta = V3 r g (updateColorChannel b delta)

editPrimitiveColor :: Int -> EventM Name BDPTRenderState ()
editPrimitiveColor delta = do
  (MkBDPTRenderState _ img scene primitiveIdx colorChannel) <- get
  case primitiveIdx of
    Nothing -> return ()
    Just idx ->
      let (Sphere center radius sphere_color) = primitives scene !! idx
          updatedSphere = Sphere center radius (updateColor colorChannel sphere_color delta)
          updatedScene = updateSphere scene idx updatedSphere
      in
        put $ MkBDPTRenderState 0 img updatedScene primitiveIdx colorChannel

accumulateImg :: Image -> Image -> Int -> Image
accumulateImg old new sampleIdx =
  let 
    si = fromIntegral sampleIdx :: Float
    img1' = map (\(V3 r1 g1 b1)  -> V3 (r1 * si) (g1 * si) (b1 * si)) old
    img2' = zipWith (+) img1' new
    accumulated = map (\(V3 r2 g2 b2) -> V3 (r2 / (si + 1)) (g2 / (si + 1)) (b2 / (si + 1))) img2'
  in
    accumulated

handleEvent :: BrickEvent Name Tick -> EventM Name BDPTRenderState ()
handleEvent (AppEvent Tick) = do
  (MkBDPTRenderState sampleIdx img scene primitiveIdx colorChannel) <- get
  if sampleIdx >= spp then
    return ()
  else do
    let newImg = Scene.render scene width height sampleIdx
    let accumulatedImg = accumulateImg img newImg sampleIdx
    put $ MkBDPTRenderState (sampleIdx+1) accumulatedImg scene primitiveIdx colorChannel
  
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = updateChannelColor CCR
handleEvent (VtyEvent (V.EvKey (V.KChar 'g') [])) = updateChannelColor CCG
handleEvent (VtyEvent (V.EvKey (V.KChar 'b') [])) = updateChannelColor CCB
handleEvent (VtyEvent (V.EvKey V.KDown       [])) = editPrimitiveColor (-8)
handleEvent (VtyEvent (V.EvKey V.KUp         [])) = editPrimitiveColor 8
handleEvent (MouseUp ClickableImage _ (Location (x, y))) = do
  (MkBDPTRenderState sampleIdx img scene _ colorChannel) <- get
  -- Each "pixel" is made up of 2 chars, so divide x coord by 2
  let coords = (x `div` 2, height-y)
  let primitiveIdx = rayCastPrimitive test2 coords (width, height)
  put $ MkBDPTRenderState sampleIdx img scene primitiveIdx colorChannel
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
