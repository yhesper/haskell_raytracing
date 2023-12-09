module Scene
    ( bdptApp
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Linear.V3 (V3)

data Ray = R {
    origin :: (V3 Float),
    direction :: (V3 Float)
}

data Intersction = I {
    t   :: Float,
    prim_idx   :: Int,
    normal :: (V3 Float),
    -- bsdf  :: BSDF a
    color :: (V3 Float)
}

class Primitive a where
  intersect :: a -> Ray -> Intersction
  volume :: a -> Float
  area :: a -> Float

data Sphere = Sphere {
    center :: (V3 Float),
    radius :: Float,
    sphere_color  :: (V3 Float)
}

instance Primitive Sphere where
  intersect s r = I {
    t = 0,
    prim_idx = 0,
    normal = V3 0 0 0,
    color = V3 0 0 0
  }
  volume s = 0
  area s = 0

data Triangle = Triangle {
    v0 :: (V3 Float),
    v1 :: (V3 Float),
    v2 :: (V3 Float),
    tri_color :: (V3 Float)
}

instance Primitive Triangle where
  intersect t r = I {
    t = 0,
    prim_idx = 0,
    normal = V3 0 0 0,
    color = V3 0 0 0
  }
  volume t = 0
  area t = 0

data Mesh = Mesh {
    vertices :: [(V3 Float)],
    indices  :: [Int],
    normals  :: [(V3 Float)],
    colors   :: [(V3 Float)]
}

-- data Scene = Scene {
--     primitives :: [Primitive]
--     -- lights     :: [Primitive]
-- }


-- trace :: Ray -> Scene -> (V3 Float)
-- trace r s = V3 0 0 0

-- do this later
-- class BSDF a where 



