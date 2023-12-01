module Lib
    ( someFunc
    ) where
import System.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- A Haskell program that does path tracing
-- 1. Generate a random number
-- 2. Generate a random vector
-- 3. Generate a random color
-- 4. Generate a random point
-- 5. Generate a random ray
-- 6. Generate a random sphere
-- 7. Generate a random hitable list
-- 8. Generate a random camera
-- 9. Generate a random material
-- 10. Generate a random lambertian
-- 11. Generate a random metal