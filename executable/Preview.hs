{-# LANGUAGE TypeOperators #-}
module Main where


import Raytracer
import Data.Time
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Algorithms.Matrix as R

dummyCam :: Camera
dummyCam = createCamera (1,0,0) (0,0,0) (0,1,0) (45)
              
--R.Array R.U (R.Z R.:. Int) Double
main :: IO ()
main = do
    let hight = 500
    let width = 500
    let spherePos1' = R.fromListUnboxed (R.ix2 1 3) [10.0,0.0,0.0] 
    spherePos2' <- R.mmultP spherePos1' (rotMatY  (0-pi/4))
    spherePos3' <- R.mmultP spherePos1' (rotMatY  (pi/4))   
    
    --let spherePos2' = R.mmultP $ (R.computeUnboxedS (R.transpose (R.reshape (R.ix2 1 3) spherePos1))) (rotMatZ  (0-pi/4))
    let spherePos1 = R.computeUnboxedS $ R.slice (R.transpose spherePos1') (R.Any R.:. (0::Int))
    let spherePos2 = R.computeUnboxedS $ R.slice (R.transpose spherePos2') (R.Any R.:. (0::Int))
    let spherePos3 = R.computeUnboxedS $ R.slice (R.transpose spherePos3') (R.Any R.:. (0::Int))

    
    let  obj = ([EntO (createPlane vDown vUp (0.0,150.0,0.0) 1 0),
                EntO (v2Sphere spherePos1 (t2c Blue) 2.0  0 16),
                EntO (v2Sphere  spherePos2 (t2c White) 2.0 0 16),
                EntO (v2Sphere  spherePos3 (t2c Red) 2.0 0 16),
                EntL (createLight (6.0,0.0,0.0) (t2c White))])
    putStrLn $ "Starting trace on a " ++ show width ++ "x" ++ show hight ++ " ..."
    t0 <- getCurrentTime
    let image = trace2Array obj dummyCam (width ,hight) 2 --"test.bmp"
    let imageFinal = rot180 image
    
    t1 <- getCurrentTime
    putStrLn $ "Trace is done (in "++ show (diffUTCTime t1 t0) ++") creating image named " ++ "test.bmp"


-- | shamlessly stolen from the repa Tutorial to show that you can postprosess our traced images
-- https://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial
rot180 :: (R.Source r e) => R.Array r R.DIM2 e -> R.Array R.D R.DIM2 e
rot180 g = R.backpermute e flop g
    where
        e@(R.Z R.:. x R.:. y )   = R.extent g
        flop (R.Z R.:. i         R.:. j         ) =
             (R.Z R.:. x - i - 1 R.:. y - j - 1 )    
    
 
rotMatY :: Double -> R.Array R.U R.DIM2 Double
rotMatY angle = R.fromListUnboxed (R.ix2 3 3)[
                              cos(angle)::Double, 0.0, sin(angle)::Double,
                              0.0, 1.0, 0.0,
                              (0.0-sin(angle))::Double, 0.0, cos(angle)::Double]
rotMatZ :: Double -> R.Array R.U R.DIM2 Double
rotMatZ angle =(R.fromListUnboxed (R.ix2 3 3) [cos(angle)::Double, (0.0-sin(angle))::Double, 0.0,
                                              sin(angle)::Double,cos(angle)::Double, 0.0,
                                              0.0               ,0.0               ,1.0])