module Scene
    ( Vector2(..)
    , Vector3(..)
    , Ray(..)
    , Camera(..)
    , Intersection(..)
    , Primitive(..)
    , Sphere(..)
    , Triangle(..)
    , Mesh(..)
    , Scene(..)
    ) where

{-# LANGUAGE OverloadedStrings #-}

data Vector2 = Vector2 {
    u :: Float,
    v :: Float
} deriving (Eq, Show)

data Vector3 = Vector3 {
    x :: Float,
    y :: Float,
    z :: Float
} deriving (Eq, Show)

data Ray = Ray {
    origin :: Vector3,
    direction :: Vector3
} deriving (Eq, Show)

data Camera = Camera {
    position :: Vector3,
    look_at :: Vector3,
    up :: Vector3,
    fov :: Float
} deriving (Eq, Show)

data Intersection = Intersection {
    t :: Float,
    prim_idx :: Int,
    normal :: Vector3,
    color :: Vector3
} deriving (Eq, Show)


class Primitive a where
  intersect :: a -> Ray -> Intersection
  volume :: a -> Float
  area :: a -> Float

data Sphere = Sphere {
    center :: Vector3,
    radius :: Float,
    sphere_color  :: Vector3
} deriving (Eq, Show)

data Triangle = Triangle {
    v1 :: Vector3,
    v2 :: Vector3,
    v3 :: Vector3,
    triangle_color :: Vector3
} deriving (Eq, Show)

data Mesh = Mesh {
    vertices :: [Vector3],
    indices  :: [Int],
    normals  :: [Vector3],
    colors   :: [Vector3]
} deriving (Eq, Show)

data Scene = Scene {
    primitives :: [Sphere]
    -- lights     :: [Primitive]
} deriving (Eq, Show)


-- trace :: Ray -> Scene -> (V3 Float)
-- trace r s = V3 0 0 0

-- do this later
-- class BSDF a where 



