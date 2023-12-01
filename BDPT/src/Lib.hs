module Lib
    ( bdptApp
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Brick
import qualified Graphics.Vty as V

data Tick = Tick

-- | Named resources
type Name = ()
type Image = ()

data Pixel = Red | Green | Blue

initialState :: Image
initialState = ()

height :: Int
height = 3
width :: Int
width = 3

data Coord = V2 Int Int

drawGrid :: Image -> Widget Name
drawGrid img = vBox rows
      where 
       -- draw all the cells in the grid.
       -- a cell represent  disk, peg or Empty.
       rows =  [ hBox $ cellsInRow r | r <- [height-1,height-2..0]]
       cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
       drawCoord = drawPixel . pixelAt 
       pixelAt (V2 x y)
         | x == 0 && y == 0 = Red
         | otherwise = Green

drawPixel :: Pixel -> Widget Name 
drawPixel Red = withAttr (attrName "red") cw
drawPixel Green = withAttr (attrName "green") cw
drawPixel Blue = withAttr (attrName "blue") cw

cw :: Widget Name
cw = str "  "

drawUI :: Image -> [Widget Name]
drawUI im = [drawGrid im]

handleEvent :: BrickEvent Name Tick -> EventM Name Image ()
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent _ = return ()

attributeMap :: AttrMap
attributeMap = attrMap
  V.defAttr [
    (attrName "red", fg V.red),
    (attrName "green", fg V.green),
    (attrName "blue", fg V.blue)
  ]

bdptApp :: IO ()
bdptApp = do
  -- Initialize Brick app with default settings
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
