module Scene
    ( Ray(..)
    , Camera(..)
    , Intersection(..)
    , Primitive(..)
    , Sphere(..)
    , Triangle(..)
    , Mesh(..)
    , Scene(..)
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


class Primitive a where
  intersect :: a -> Ray -> Intersection
  volume :: a -> Float
  area :: a -> Float

data Sphere = Sphere {
    center :: V3 Float,
    radius :: Float,
    sphere_color  :: V3 Float
} deriving (Eq, Show)

instance Primitive Sphere where
  intersect s r = 
    let
      l = (center s) - (origin r)
      tca = l `v3Dot` (direction r)
      d2 = (l `v3Dot` l) - (tca * tca)
      r2 = (radius s) * (radius s)
      is_intersecting = (d2 <= r2)      
    in 
      if is_intersecting then
        let 
          thc = sqrt(r2 - d2)
          t0 = tca - thc
          t1 = tca + thc
        in  
          Intersection {
            t = min t0 t1,
            prim_idx = 0,
            normal = ((origin r) + ((direction r) `v3Times` (min t0 t1))) - (center s),
            color = sphere_color s
          }
      else
        Intersection {
            t = -1,
            prim_idx = 0,
            normal = V3 0 0 0,
            color = V3 0 0 0
          }
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

test1 :: Scene
test1 = Scene [Sphere (V3 1 1 0) 1 (V3 1 0 0), Sphere (V3 0 0 0) 1 (V3 0 1 0), Sphere (V3 2 0 0) 1 (V3 0 0 1)]


-- trace :: Ray -> Scene -> (V3 Float)
-- trace r s = V3 0 0 0

-- do this later
-- class BSDF a where 



