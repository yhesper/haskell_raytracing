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
    test2,
    v3Times,
    updateSphere
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Linear.V3
import Linear.Matrix
import System.Random

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

v3Cross (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

v3Norm :: V3 Float -> Float
v3Norm (V3 x y z) = sqrt (x*x + y*y + z*z)

v3Normalize :: V3 Float -> V3 Float
v3Normalize v = v `v3Div` (v3Norm v)

dist :: V3 Float -> V3 Float -> Float
dist (V3 x1 y1 z1) (V3 x2 y2 z2) =
  sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)

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

-- data AreaLight = AreaLight
--   { lightPosition :: V3 Float  -- Position of the light source
--   , lightNormal :: V3 Float   -- Normal vector of the light surface
--   , lightColor :: V3 Float      -- Color of the light
--   , lightIntensity :: Float  -- Intensity of the light
--   , lightSize :: Float       -- Size of the light source
--   } deriving (Show)

data PointLight = PointLight
  { lightPosition :: V3 Float  -- Position of the light source
  , lightColor :: V3 Float      -- Color of the light
  , lightIntensity :: Float  -- Intensity of the light
  } deriving (Show)


data Scene = Scene {
    primitives :: [Sphere]
    -- light     ::  AreaLight
} deriving (Show)
-- al = AreaLight (V3 0 (sphere_y+5) 0) (V3 0 (-1) 0) (V3 1 1 1) 1 1


sphere_y :: Float
sphere_y = -1.8
test2 :: Scene
-- test2 = Scene [bottom, backWall, leftWall, rightWall, top, area_light, Sphere (V3 1 (sphere_y+1) 0) 1 (V3 1 0 0), Sphere (V3 0 sphere_y 0) 1 (V3 0 1 0), Sphere (V3 2 sphere_y 0) 1 (V3 0 0 1)] al
test2 = Scene [bottom, backWall, leftWall, rightWall, top, area_light, Sphere (V3 1 (sphere_y+1) 0) 1 (V3 1 0 0), Sphere (V3 0 sphere_y 0) 1 (V3 0 1 0), Sphere (V3 2 sphere_y 0) 1 (V3 0 0 1)]
rightWall = Sphere (V3 (1e5) 50 (-2e5)) 1.3e5 (V3 0.25 0.25 0.75)
leftWall = Sphere (V3 (-1e5) 50 (-2e5)) 1.3e5 (V3 0.75 0.25 0.25)
backWall = Sphere (V3 0 00 (-2e5)) 1.3e5 (V3 0.75 0.75 0.75)
bottom = Sphere (V3 50 (-1.08e5) 150) 1e5 (V3 0.75 0.75 0.75)
top = Sphere (V3 0 (1e5) (-2.2e5)) 1.6e5 (V3 0.75 0.75 0.75)
area_light = Sphere (V3 0 (sphere_y+5) 0) 0.5 (V3 1 1 1)



updateSphere :: Scene -> Int -> Sphere -> Scene
updateSphere s prim_id new_prim =
  let
    (left, right) = splitAt prim_id (primitives s)
  in
    -- Scene (left ++ [new_prim] ++ (tail right)) (light s)
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
    intersections = mapWithIndex (flip (intersect r)) (primitives s)
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
    ww = fromIntegral w :: Float
    hh = fromIntegral h :: Float
    aspect_ratio = ww / hh
    viewport_height = 2.0
    viewport_width = viewport_height * aspect_ratio
    focal_length = 1.0
    camera_center = V3 0 0 4
    viewport_upper_left = camera_center - V3 (viewport_width / 2) (viewport_height / 2) focal_length
    pixel00_loc = viewport_upper_left + V3 (viewport_width / ww) 0 0 + V3 0 (viewport_height / hh) 0
    pixel_delta_u = (viewport_width / ww)
    pixel_delta_v = (viewport_height / hh)
  in
    (camera_center, pixel00_loc, (pixel_delta_u, pixel_delta_v))

raygen :: CameraFrame -> Int -> Int -> Ray
raygen (camera_center, pixel00_loc, (pixel_delta_u, pixel_delta_v)) x y =
  let pixel_loc = pixel00_loc + (V3 (pixel_delta_u * (fromIntegral x + 0.5)) (pixel_delta_v * (fromIntegral y + 0.5)) 0)
      ray = Ray camera_center (v3Normalize (pixel_loc - camera_center))
  in
    ray

render :: Scene -> Int -> Int -> Int -> [V3 Float]
render s w h sampleIdx =
  let cameraFrame = setupCameraFrame w h
  in
    do
      y <- [0..h-1]
      x <- [0..w-1]
      let ray = raygen cameraFrame x y
      let seed = sampleIdx * 32 * w * h * 3 + y * w * 3 + x * 3
      return $ traceRay ray s 2 seed

calculatePointLightIntensity :: PointLight -> V3 Float -> Float
calculatePointLightIntensity light point =
  let distance = dist (lightPosition light) point
  in lightIntensity light / (distance * distance)

traceRay :: Ray -> Scene -> Int -> Int -> V3 Float
traceRay r s d seed =
  let
    intersections = mapWithIndex (flip (intersect r)) (primitives s)
    valid_intersections = filter (\i -> case i of
                                          Just _ -> True
                                          Nothing -> False) intersections
  in
    if length valid_intersections > 0 then
      do
        let mi = minimum valid_intersections
        let hitColor = maybe (V3 0 0 0) color mi
        let hitPoint = origin r + (direction r `v3Times` maybe 0 t mi)
        let hitNormal = maybe (V3 0 0 0) normal mi
        let light = PointLight (V3 0 (sphere_y + 5) 0) (V3 1 1 1) 10
        let rm = rotationMatrix hitNormal
        let newDirection = randomDirection $ mkStdGen (seed + d)
        let newDirectionRotated = rm !* newDirection
        let newRay = Ray (hitPoint + newDirectionRotated `v3Times` 1e-3) (v3Normalize newDirectionRotated)
        let lightDirection = v3Normalize (lightPosition light - hitPoint)
        let color = hitColor -- `v3Times` max (hitNormal `v3Dot` lightDirection) 0.0
        if d == 0 then
          color * lightColor light `v3Times` lightIntensity light
        else
          color * traceRay newRay s (d - 1) seed
    else
      V3 0 0 0

randomDirection :: StdGen -> V3 Float
randomDirection gen =
  let
    (u, gen1) = random gen
    (v, gen2) = random gen1
    sx = 2 * u - 1
    sy = 2 * v - 1
    r = if abs sx > abs sy then sx else sy
    theta = if abs sx > abs sy then pi / 4 * (sy / sx) else pi / 2 - pi / 4 * (sx / sy)
    x = r * cos theta
    y = r * sin theta
    z = sqrt (max 0 (1 - x * x - y * y))
  in
    V3 x y z

rotationMatrix :: V3 Float -> M33 Float
rotationMatrix (V3 xx yy zz)
  | abs xx <= abs yy && abs xx <= abs zz = let
                                          z = V3 xx yy zz
                                          h = V3 1 yy zz
                                          y = v3Normalize (h `v3Cross` z)
                                          x = v3Normalize (z `v3Cross` y)
                                        in
                                          V3 x y z
  | abs yy <= abs xx && abs yy <= abs zz = let
                                          z = V3 xx yy zz
                                          h = V3 xx 1 zz
                                          y = v3Normalize (h `v3Cross` z)
                                          x = v3Normalize (z `v3Cross` y)
                                        in
                                          V3 x y z
  | otherwise = let
                  z = V3 xx yy zz
                  h = V3 xx yy 1
                  y = v3Normalize (h `v3Cross` z)
                  x = v3Normalize (z `v3Cross` y)
                in
                  V3 x y z
