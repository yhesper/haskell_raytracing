{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Linear.V3
import Scene
import qualified Data.Map as M 

main :: IO ()
main = runTests 
  [ resultTests ]

resultTests ::  Score -> TestTree
resultTests sc = testGroup "result" 
  [ scoreTest ((\_ -> test_intersection),  (), True, 10, "test-intersection-1")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

test_intersection = do
  let sphereYellow = Sphere (V3 0 0 0 ) 1 (V3 0 1 1)
  let r = Ray (V3 0 0 4) (V3 0 0 (-1)) 1e99
  let intersectionMaybe = Scene.intersect r sphereYellow 0
  case intersectionMaybe of
    Just intersection -> abs ((t intersection) - 3) < 1e-3
    Nothing -> False
