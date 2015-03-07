{-# LANGUAGE TypeOperators #-}
import Control.Monad.State
import qualified Data.Array.Repa                  as R
import qualified Data.Array.Repa.Eval             as R
import qualified Data.Array.Repa.Unsafe           as R
import Data.Array.Repa.IO.BMP
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
             , color= (255,0,0) --(R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0}
             , Object{shape =dummySphere2
             , color=(255,255,255) -- (R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0}]



dummyVec1 :: DoubleVector
dummyVec1 = R.fromListUnboxed (R.ix1 3) [1,1,1]


dummyVec2 :: DoubleVector
dummyVec2 = R.fromListUnboxed (R.ix1 4) [5,0,0,0]


main :: IO ()
main = do
    let w = dummyWorld 
    col <- trace dummyWorld dummyRay 0
    let image = R.fromListUnboxed (R.ix2 100 100) [ col | x <- [1..100], y <- [1..100]] 
    
    writeImageToBMP ("./test.bmp") image

--                 
trace :: World -> Ray -> Depth -> IO Color
trace w r@Ray{dir = dir, point = pnt} d = do
    case d of
        5 -> return $ (0,0,0)     -- byt 5an till dynamisk?
        _ -> do 
            test <-intersectWorld r w 
            case test of
                Nothing -> return $ (0,0,0)
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
                    return $ calcFinalCol emittance reflCol brdf
                    
                    {-$ R.computeUnboxedS $ 
                        R.zipWith (+) emittance $
                        calcReflCol brdf reflCol-}
                    
                 
            
calcFinalCol :: Color -> Color -> Double ->Color
calcFinalCol (er,eg,eb) (rr,rg,rb) brf = (calc er rr,calc eg rg,calc eb rb)  --R.computeUnboxedS$ R.map round $ R.map (b*) $ R.map fromIntegral rc
    where calc a b = a + round (brf * fromIntegral b)
calcHemispherePoint :: Int -> Int -> DoubleVector -> DoubleVector
calcHemispherePoint = undefined
            
            
calcNorm =undefined 




randVec:: DoubleVector ->  Double -> StdGen -> IO DoubleVector
randVec v spawnFrustum randG = do
                    --randG <- getStdGen
                    --calculate the length of "Normal" in the XY-plane
                    let lengthxy = (v R.! (R.Z R.:. 0)) * (v R.! (R.Z R.:. 0)) + (v R.! (R.Z R.:. 1)) * (v R.! (R.Z R.:. 1)) -- sqared length                    
                    let alphaxy = 0
                    lengthxy2 <- alphaxyAng v lengthxy
                   
                    --calculate the length of "Normal" in the XZ-plane
                    let lengthxz = (v R.! (R.Z R.:. 0)) * (v R.! (R.Z R.:. 0)) + (v R.! (R.Z R.:. 2)) * (v R.! (R.Z R.:. 2)) -- sqared length
                    alphaxz <- alphaxzAng v lengthxz
                    



                    let vector_x = (R.fromListUnboxed (R.ix2 1 4) [1,0,0,1])            
                    -- pick angle and make rotation matrix
                    let (rand',randG2) = randomR (-1000,1000) randG -- <- undefined
                    let rand = (rand'/1000)::Double
                    let tempRand = rand * spawnFrustum - spawnFrustum/2


                     --pick a new angle and make another rotation matrix
                    -- rot z
                    let mat = (R.fromListUnboxed (R.ix2 4 4)[cos(tempRand),0-sin(tempRand),0, 0,               
                                                                sin(tempRand), cos(tempRand), 0, 0,
                                                                0,0,1,0,
                                                                0,0,0,1])                

                    vector_x2 <- mmultP vector_x mat
                    let (rand2',_) = randomR (-1000,1000) randG2 -- <- undefined
                    let rand2 = rand2'/1000
                    let tempRand2 = rand2 * (2::Double) * (3.14::Double) - (3.14::Double)
                     -- rot x
                    let mat2 = (R.fromListUnboxed (R.ix2 4 4) [1,0,0,0,                                                
                                                          0, cos(tempRand2), 0-sin(tempRand2),0,
                                                          0, sin(tempRand2), cos(tempRand2),0,
                                                          0,0,0,1])
                                                        
                    
                    vector_x3 <- mmultP vector_x2 mat


                    let mat3 = R.fromListUnboxed (R.ix2 4 4) [cos(alphaxy), 0-sin(alphaxy), 0, 0,                 -- //rot z
                                                sin(alphaxy),cos(alphaxy), 0, 0,
                                                0, 0, 1, 0,
                                                0, 0, 0, 1]
                                                
                    vector_x4 <- mmultP vector_x3  mat3;

                    let mat4 =  R.fromListUnboxed (R.ix2 4 4)[
                              cos(alphaxz), 0, sin(alphaxz), 0,
                              0, 1, 0, 0,
                              0-sin(alphaxz), 0, cos(alphaxz), 0,
                              0, 0, 0, 1]
                                
                    vector_x5 <- mmultP vector_x4 mat4
                    
                    return $ R.fromListUnboxed (R.ix1 3) [ (vector_x5 R.! (R.Z R.:. 0 R.:. 0))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3)) ,(vector_x5 R.! (R.Z R.:. 0 R.:. 1))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3)),(vector_x5 R.! (R.Z R.:. 0 R.:. 2))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3))] 
    where 
        alphaxyAng:: DoubleVector -> Double -> IO Double
        alphaxyAng v a = case a == 0 of
                True -> return (0.0::Double)
                False -> do 
                           let lengthxy2 = sqrt(a) -- real length
                           --calculate the angle between the x-axis and "Normal" in the XY-plane
                           let alphaxy = asin((v R.! (R.Z R.:. 2) / lengthxy2))
                           return alphaxy
        alphaxzAng:: DoubleVector -> Double -> IO Double
        alphaxzAng v a =
                   case a == 0 of
                        True -> return (0.0) -- 90 degrees
                            
                        False ->  do
                            let lengthxz = sqrt(a) -- real length
                            -- calculate the angle between the x-axis and "Normal" in the XZ-plane
                            let alphaxz = asin((v R.! (R.Z R.:. 2)) / lengthxz) -- sin alpha = z / hypotenuse
                            return alphaxz
{-

    yRot <- mmultP  (R.fromListUnboxed (R.ix2 1 3) [1,0,0]) (R.fromListUnboxed (R.ix2 3 3) [cos r2, 0, sin r2
                                                            ,0,1,0
                                                            ,-sin r2,0,cos r2]) 
    zRot <- mmultP yRot (R.fromListUnboxed (R.ix2 3 3) [cos r3, -sin r3, 0
                                                        ,sin r3,cos r3,0
                                                        ,0,0,0]) 
    
    xyzRotNorm  <- mmultP zRot (R.fromListUnboxed (R.ix2 3 3) [1,0,0
                                                             ,0,cos r1,-sin r1
                                                             ,0,sin r1,cos r1])
    
    let yzRotNormOx = R.fromListUnboxed (R.ix1 3) [0,(v R.! (R.Z R.:. 1)),(v R.! (R.Z R.:. 2))]
    let yzRotNormOy = R.fromListUnboxed (R.ix1 3) [(v R.! (R.Z R.:. 0)),0,(v R.! (R.Z R.:. 2))]
    let yzRotNormOz = R.fromListUnboxed (R.ix1 3) [(v R.! (R.Z R.:. 0)),(v R.! (R.Z R.:. 1)),0]
    x <- (dotProd (R.fromListUnboxed (R.ix1 3) [1,0,0]) yzRotNormOx)
    y <- (dotProd (R.fromListUnboxed (R.ix1 3) [0,1,0]) yzRotNormOy)
    z <- (dotProd (R.fromListUnboxed (R.ix1 3) [0,0,1]) yzRotNormOz)
    let xAngle = acos (v R.! (R.Z R.:. 0))
    let yAngle = acos (v R.! (R.Z R.:. 1))
    let zAngle = acos (v R.! (R.Z R.:. 2))
    rotYyzRotNorm <- mmultP xyzRotNorm (R.fromListUnboxed (R.ix2    3 3) [cos yAngle, 0, sin yAngle
                                                        ,0,1,0
                                                        ,-sin yAngle,0,cos yAngle])
    rotYZyzRotNorm <-  mmultP rotYyzRotNorm (R.fromListUnboxed (R.ix2 3 3) [cos zAngle, -sin zAngle, 0
                                                          ,sin zAngle,cos zAngle,0
                                                          ,0,0,0])
    rotYZXyzRotNorm <- mmultP rotYZyzRotNorm (R.fromListUnboxed (R.ix2 3 3) [1,0,0
                                                     ,0,cos xAngle,-sin xAngle
                                                     ,0,sin xAngle,cos xAngle])
                                                    
    
                                                     
    return $ R.fromListUnboxed (R.ix1 3) [xAngle,yAngle,zAngle]                                        
        --where
            --zeroVec = R.fromListUnboxed (R.ix1 3) [0,0,0]
-}
mmultP  :: R.Array R.U R.DIM2 Double -> R.Array R.U R.DIM2 Double -> IO (R.Array R.U R.DIM2 Double)

mmultP arr brr 
 = [arr, brr] `R.deepSeqArrays` 
   do   trr      <- transpose2P brr
        let (R.Z R.:. h1  R.:. _)  = R.extent arr
        let (R.Z R.:. _   R.:. w2) = R.extent brr
        R.computeP 
         $ R.fromFunction (R.Z R.:. h1 R.:. w2)
         $ \ix   -> R.sumAllS 
                  $ R.zipWith (*)
                        (R.unsafeSlice arr (R.Any R.:. (row ix) R.:. R.All))
                        (R.unsafeSlice trr (R.Any R.:. (col ix) R.:. R.All))


transpose2P::R.Array R.U R.DIM2 Double -> IO (R.Array R.U R.DIM2 Double)

transpose2P arr = arr `R.deepSeqArray`
   do   R.computeUnboxedP 
         $ R.unsafeBackpermute new_extent swap arr
 where  swap (R.Z R.:. i R.:. j)      = R.Z R.:. j R.:. i
        new_extent              = swap (R.extent arr)     


row :: R.DIM2 -> Int
row (R.Z R.:. r R.:. _) = r 

col :: R.DIM2 -> Int
col (R.Z R.:. _ R.:. c) = c       
            