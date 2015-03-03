{-# LANGUAGE TypeOperators #-}
import Control.Monad.State
import qualified Data.Array.Repa as R
import System.Random

import Ray
import World
import Vector


dummyRay :: Ray
dummyRay = Ray { dir =  R.fromListUnboxed (R.ix1 4) [1,0,0,0] 
                ,point = R.fromListUnboxed (R.ix1 3) [0,0,0]
               }
                
dummySphere :: Sphere
dummySphere = Sphere {
                position =  R.fromListUnboxed (R.ix1 4) [10,0,0,0] 
                ,radius = 1.0
            }
            
dummySphere2 :: Sphere
dummySphere2 = Sphere {
                position =  R.fromListUnboxed (R.ix1 4) [5,0,0,0] 
                ,radius = 4.0
            }
            
dummyWorld :: World
dummyWorld = [ Object{shape =dummySphere
             , color=(R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0}
             , Object{shape =dummySphere2
             , color=(R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0}]



dummyVec1 :: DoubleVector
dummyVec1 = R.fromListUnboxed (R.ix1 4) [0,0,0,0]


dummyVec2 :: DoubleVector
dummyVec2 = R.fromListUnboxed (R.ix1 4) [5,0,0,0]


--                 
trace :: World -> Ray -> Depth -> IO Color
trace w r@Ray{dir = dir, point = pnt} d = do
    case d of
        5 -> return $ R.fromListUnboxed (R.ix1 4) [0,0,0,0]     -- byt 5an till dynamisk?
        _ -> do 
            test <-intersectWorld r w 
            case test of
                Nothing -> return $ R.fromListUnboxed (R.ix1 4) [0,0,0,0]
                (Just (obj,hitp)) -> do
                    let emittance = color obj
                    rand <- getStdGen
                    let randInt = next rand 
                    let randInt2 = fst $ next (snd randInt)
                    let norm = sphereNormal (shape obj) hitp
                    let newDir =  calcHemispherePoint (fst randInt) randInt2 norm
                    cos_theta <- dotProd newDir norm
                    let brdf = 2 * (reflectance obj) * cos_theta
                    reflCol <- trace w (Ray{dir=newDir, point = hitp}) (d+1)
                    return $ R.computeUnboxedS $ 
                        R.zipWith (+) emittance $
                        R.computeUnboxedS$ R.map (brdf*) reflCol
                    
                 
            
            

calcHemispherePoint :: Int -> Int -> DoubleVector -> DoubleVector
calcHemispherePoint = undefined
            
            
            
            
            