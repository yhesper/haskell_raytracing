module Lib
    ( bdptApp
    ) where
import Data.Vector.V3
{-# LANGUAGE OverloadedStrings #-}


data Ray = R {
    origin :: Vector3,
    dir  :: Vector3,
}

data Intersction = I {
    t   :: Float,
    prim_idx   :: Int,
    normal :: Vector3,
    -- bsdf  :: BSDF a
    color :: Vector3
}

class Primitive a where
  intersect :: a -> Ray -> Intersction
  volume :: a -> Float
  area :: a -> Float

data Sphere = Sphere {
    center :: Vector3,
    radius :: Float,
    color  :: Vector3
}

instance Primitive Sphere where
  intersect s r = I {
    t = 0,
    prim_idx = 0,
    normal = Vector3 0 0 0,
    color = Vector3 0 0 0
  }
  volume s = 0
  area s = 0

data Triangle = Triangle {
    v0 :: Vector3,
    v1 :: Vector3,
    v2 :: Vector3,
    color :: Vector3
}

instance Primitive Triangle where
  intersect t r = I {
    t = 0,
    prim_idx = 0,
    normal = Vector3 0 0 0,
    color = Vector3 0 0 0
  }
  volume t = 0
  area t = 0

Data Mesh = Mesh {
    vertices :: [Vector3],
    indices  :: [Int],
    normals  :: [Vector3],
    colors   :: [Vector3]
}

Data Scene = Scene {
    primitives :: [Primitive],
    -- lights     :: [Primitive]
}


trace :: Ray -> Scene -> Vector3
trace r s = Vector3 0 0 0

-- do this later
-- class BSDF a where 



