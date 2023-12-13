module Scene
    ( Ray(..)
    , Camera(..)
    , Intersection(..)
    , Primitive(..)
    , Sphere(..)
    , Triangle(..)
    , Mesh(..)
    , Scene(..),
    render,
    rayCastPrimitive,
    test1,
    test2
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Linear.V3


-- data Vector2 = Vector2 {
--     u :: Float,
--     v :: Float
-- } deriving (Eq, Show)

-- data Vector3 = Vector3 {
--     x :: Float,
--     y :: Float,
--     z :: Float
-- } deriving (Eq, Show)

v3Div :: V3 Float -> Float -> V3 Float
v3Div (V3 x y z) s = V3 (x / s) (y / s) (z / s)

v3Times (V3 x y z) s = V3 (x * s) (y * s) (z * s)

v3Dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

v3Norm :: V3 Float -> Float
v3Norm (V3 x y z) = sqrt (x*x + y*y + z*z)

v3Normalize :: V3 Float -> V3 Float
v3Normalize v = v `v3Div` (v3Norm v)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = map (\(index, element) -> f index element) (zip [0..] xs)

data Ray = Ray {
    origin :: V3 Float,
    direction :: V3 Float
} deriving (Eq, Show)

data Camera = Camera {
    position :: V3 Float,
    look_at :: V3 Float,
    up :: V3 Float,
    fov :: Float
} deriving (Eq, Show)

data Intersection = Intersection {
    t :: Float,
    prim_idx :: Int,
    normal :: V3 Float,
    color :: V3 Float
} deriving (Eq, Show)

instance Ord Intersection where
  compare i1 i2 = compare (t i1) (t i2)


class Primitive a where
  intersect :: Ray -> a -> Int -> Maybe Intersection
  volume :: a -> Float
  area :: a -> Float

data Sphere = Sphere {
    center :: V3 Float,
    radius :: Float,
    sphere_color  :: V3 Float
} deriving (Eq, Show)

instance Primitive Sphere where
  intersect r s i =
    let
      l = (center s) - (origin r)
      tca = l `v3Dot` (direction r)
      d2 = (l `v3Dot` l) - (tca * tca)
      r2 = (radius s) * (radius s)
    in
      if (d2 <= r2) && not (tca < 0)then
        let
          thc = sqrt (r2 - d2)
          t0 = tca - thc
          t1 = tca + thc
          intersect_t = if (min t0 t1) >= 0 then min t0 t1 else max t0 t1
        in
          if intersect_t < 0 then
            Nothing
          else
            Just Intersection {
              t = intersect_t,
              prim_idx = i,
              normal = v3Normalize (((origin r) + ((direction r) `v3Times` intersect_t)) - (center s)),
              color = sphere_color s
            }
      else
        Nothing
  volume s = 4/3 * pi * (radius s) * (radius s) * (radius s)
  area s = 4 * pi * (radius s) * (radius s)

data Triangle = Triangle {
    v1 :: V3 Float,
    v2 :: V3 Float,
    v3 :: V3 Float,
    triangle_color :: V3 Float
} deriving (Eq, Show)


data Mesh = Mesh {
    vertices :: [V3 Float],
    indices  :: [Int],
    normals  :: [V3 Float],
    colors   :: [V3 Float]
} deriving (Eq, Show)

data Scene = Scene {
    primitives :: [Sphere]
    -- lights     :: [Primitive]
} deriving (Eq, Show)

test2 :: Scene
test2 = Scene [Sphere (V3 1 1 0) 1 (V3 1 0 0), Sphere (V3 0 0 0) 1 (V3 0 1 0), Sphere (V3 2 0 0) 1 (V3 0 0 1)]
-- test1 = Scene [Sphere (V3 0 0 0) 1 (V3 0 1 0)]
left = Sphere (V3 (1e5+1) 40.8 81.6) 1 (V3 0.75 0.25 0.25)
right = Sphere (V3 (-1e5+99) 40.8 81.6) 1 (V3 0.25 0.25 0.75)
back = Sphere (V3 50 40.8 1e5) 1 (V3 0.75 0.75 0.75)
front = Sphere (V3 50 40.8 (-1e5+170)) 1 (V3 0 0 0)
bottom = Sphere (V3 50 1e5 81.6) 1 (V3 0.75 0.75 0.75)
top = Sphere (V3 50 (-1e5+81.6) 81.6) 1 (V3 0.75 0.75 0.75)
test1 :: Scene
test1 = Scene [left, right, back,front, bottom, top]


updateSphere :: Scene -> Int -> Sphere -> Scene
updateSphere s prim_id new_prim = 
  let
    (left, right) = splitAt prim_id (primitives s)
  in
    Scene (left ++ [new_prim] ++ (tail right))

-- can define a function to update lighting in the same way
-- updateLight :: Scene -> Int -> Light -> Scene

-- copied from render function
-- changed pixel_loc
-- used monads
rayCastPrimitive :: Scene -> (Int, Int) -> (Int, Int) -> Maybe Int
rayCastPrimitive s (img_x, img_y) (w, h) = do
  let cameraFrame = setupCameraFrame w h
  let ray = raygen cameraFrame img_x img_y
  intersection <- traceRayPrimal ray s
  return $ prim_idx intersection



traceRayPrimal :: Ray -> Scene -> Maybe Intersection
traceRayPrimal r s =
  let
    intersections = mapWithIndex (\i p -> intersect r p i) (primitives s)
    valid_intersections = filter (\i -> case i of
                                          Just _ -> True
                                          Nothing -> False) intersections
  in
    if length valid_intersections > 0 then
      minimum valid_intersections
    else
      Nothing

type CameraFrame = (V3 Float, V3 Float, (Float, Float))

setupCameraFrame :: Int -> Int -> CameraFrame
setupCameraFrame w h =
  let   
    aspect_ratio = (fromIntegral w) / (fromIntegral h) :: Float
    viewport_height = 2.0 :: Float
    viewport_width = viewport_height * aspect_ratio
    focal_length = 1.0
    camera_center = V3 0 0 4 :: V3 Float
    viewport_upper_left = camera_center - (V3 (viewport_width / 2) (viewport_height / 2) focal_length)
    pixel00_loc = viewport_upper_left + (V3 (viewport_width / (fromIntegral w)) 0 0) + (V3 0 (viewport_height / (fromIntegral h)) 0)
    pixel_delta_u = (viewport_width / (fromIntegral w))
    pixel_delta_v = (viewport_height / (fromIntegral h))
  in
    (camera_center, pixel00_loc, (pixel_delta_u, pixel_delta_v))

raygen :: CameraFrame -> Int -> Int -> Ray
raygen (camera_center, pixel00_loc, (pixel_delta_u, pixel_delta_v)) x y =
  let pixel_loc = pixel00_loc + (V3 (pixel_delta_u * (fromIntegral x + 0.5)) (pixel_delta_v * (fromIntegral y + 0.5)) 0)
      ray = Ray camera_center (v3Normalize (pixel_loc - camera_center))
  in
    ray

render :: Scene -> Int -> Int -> [V3 Float]
render s w h =
  let cameraFrame = setupCameraFrame w h
  in
    do
      y <- [0..h-1]
      x <- [0..w-1]
      let ray = raygen cameraFrame x y
      case traceRayPrimal ray s of
        Just i -> [color i `v3Times` (max ((normal i) `v3Dot` (v3Normalize (V3 1 1 1))) 0.0)]
        Nothing -> [V3 0 0 0]