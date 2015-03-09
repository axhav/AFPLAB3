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


data Camera = Camera {
                cdir :: DoubleVector
                , cpoint :: DoubleVector
                , cup:: DoubleVector
                }

dummyCam :: Camera
dummyCam = Camera { 
                cdir =  R.fromListUnboxed (R.ix1 3) [1,0,0] 
                ,cpoint = R.fromListUnboxed (R.ix1 3) [0,0,0]
                ,cup = R.fromListUnboxed (R.ix1 3) [0,1,0]
               } 
 
dummyRay :: Ray
dummyRay = Ray { dir =  R.fromListUnboxed (R.ix1 3) [5,2.1,0] 
                ,point = R.fromListUnboxed (R.ix1 3) [0,0,0]
               }
                
dummySphere :: Shape
dummySphere = Sphere {
                spos =  R.fromListUnboxed (R.ix1 3) [10,0,0] 
                ,radius = 4.0
            }
            
dummySphere2 :: Shape
dummySphere2 = Sphere {
                spos =  R.fromListUnboxed (R.ix1 3) [5,-3,0] 
                ,radius = 2.0
            }

dummyPlane :: Shape
dummyPlane = Plane {
                ppos =  R.fromListUnboxed (R.ix1 3) [0,-2,0] 
                ,pnormal = R.fromListUnboxed (R.ix1 3) [0,1,0]
            }

dummyPlane2 :: Shape
dummyPlane2 = Plane {
                ppos =  R.fromListUnboxed (R.ix1 3) [0,0,1] 
                ,pnormal = R.fromListUnboxed (R.ix1 3) [0,0,1]
            }
            
dummyObj = Object{shape =dummySphere2
             , color=(0,255,0) -- (R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0}
           
            
dummyWorld :: World
dummyWorld = [Object{shape =dummyPlane
             , color= (100,0,0) --(R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0},
             Object{shape =dummyPlane2
             , color=(0,255,0) -- (R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0}]



dummyVec1 :: DoubleVector
dummyVec1 = R.fromListUnboxed (R.ix1 3) [1,0,0]


dummyVec2 :: DoubleVector
dummyVec2 = R.fromListUnboxed (R.ix1 4) [20,1,0,0]

cameraRay :: Camera->(Int, Int) ->Int -> Int -> Ray
cameraRay r@Camera{cdir = dir, cpoint = pnt, cup =u} (maxX',maxY') x y =
     Ray{dir= ( R.computeUnboxedS( R.zipWith (-) imagePoint pnt)), point = pnt}
        where cam_right = crossProd dir u
              maxX = (fromIntegral maxX' )
              maxY = (fromIntegral maxY' )
              normX = ((fromIntegral x ) /maxX) -0.5
              normY =  ((fromIntegral y ) /maxY) -0.5
              imagePoint = (R.zipWith (+) (R.zipWith (+) (R.zipWith (+) (R.map (normX*) u)
                    (R.map (normY*) cam_right)) pnt) dir )

cameraRay2 :: Camera->(Int, Int) -> Int -> Int -> Ray
cameraRay2 r@Camera{cdir = dir, cpoint = pnt, cup =up} (maxX',maxY') x y =
     Ray{dir= normalize( R.computeUnboxedS(R.zipWith (+) pnt imagePoint)), point = pnt}  
            where u = crossProd dir up
                  v = crossProd u dir
                  halfWidth = 100
                  halfHeight = 100
                  viewPlaneHalfWith = tan (90.0/2.0)
                  aspectRatio = 1
                  viewPlaneHalfHeight = viewPlaneHalfWith * aspectRatio
                  viewPlaneBottomLeftPoint = R.zipWith (-) ( R.zipWith (-) (dir) (R.map ( viewPlaneHalfHeight* ) v))  (R.map ( viewPlaneHalfWith* ) u)
                  xIncVector = R.map (*(2*halfWidth/ 200.0)) u
                  yIncVector = R.map (*(2*halfHeight/ 200.0)) v
                  imagePoint = R.zipWith (+) (R.zipWith (+) viewPlaneBottomLeftPoint (R.map ( (fromIntegral x)* ) xIncVector)) (R.map ( ( fromIntegral y)* ) yIncVector)

cameraRay3 :: Camera -> (Int, Int) -> Int -> Int -> Ray
cameraRay3 r@Camera{cdir = dir, cpoint = pnt, cup =up} (maxX,maxY) x y =
     Ray{dir= (R.computeUnboxedS pixelDir), point = pnt}  
            where cr = crossProd dir up
                  cu = crossProd cr dir
                  dist =(0.5 / (tan (90.0/2.0)))
                  pixelDir = (R.zipWith (+) (R.zipWith (+) (R.map (dist*) dir) (R.map ((0.5 - (fromIntegral y)/( fromIntegral(maxY -1)))*) cu) ) (R.map ((0.5 - (fromIntegral x)/( fromIntegral(maxX -1)))*) cr))
                  
main :: IO ()
main = do
    let path = "test.bmp"
    let widht = 200
    let height = 200
    putStrLn $ "Starting trace on a " ++ show widht ++ "x" ++ show height ++ " ..."
    let w = dummyWorld 
    let c = dummyCam
    --putStrLn $ show [(cameraRay2 c x y)| x <- [0..(widht-1)], y <- [0..(height-1)]]
    ls <- sequence [ trace w (cameraRay3 c (widht,height) x y) 0 | x <- [0..(widht-1)], y <- [0..(height-1)]]
    putStrLn $ "Trace is done creating image named " ++ show path
    let image = R.fromListUnboxed (R.ix2 widht height) ls
    writeImageToBMP ("./"++path) image

--                 
trace :: World -> Ray -> Depth -> IO Color
trace w r@Ray{dir = dir, point = pnt} d = do
    case d of
        1 ->do
            return $ (0,0,0)     -- byt 5an till dynamisk?
        _ -> do 
            test <-intersectWorld r w 
            case test of
                Nothing -> return $ (0,0,0)
                (Just (obj,hitp)) -> do
                    --putStrLn $ "hit"
                    let emittance = color obj
                    rand <- getStdGen
                    let norm = calcNormal obj hitp
                    newDir <-  randVec norm 3.14 rand 
                    cos_theta <- dotProd newDir norm
                    let brdf = 2 * (reflectance obj) * cos_theta
                    --putStrLn $ show newDir
                    reflCol <- trace w (Ray{dir=newDir, point = hitp}) (d+1)                    
                    return $ calcFinalCol emittance reflCol brdf
                    
                 
            
calcFinalCol :: Color -> Color -> Double ->Color
calcFinalCol (er,eg,eb) (rr,rg,rb) brf = (calc er rr,calc eg rg,calc eb rb)  --R.computeUnboxedS$ R.map round $ R.map (b*) $ R.map fromIntegral rc
    where calc a b = a + round (brf * fromIntegral b)


rotMatX :: Double -> R.Array R.U R.DIM2 Double
rotMatX angle =(R.fromListUnboxed (R.ix2 4 4) [
                                               1.0, 0.0        ,0.0             ,0.0,                                                
                                               0.0, cos(angle)::Double, (0.0-sin(angle))::Double,0.0,
                                               0.0, sin(angle)::Double, cos(angle)::Double  ,0.0,
                                               0, 0         , 0.0           ,1.0
                                              ])

rotMatY :: Double -> R.Array R.U R.DIM2 Double
rotMatY angle = R.fromListUnboxed (R.ix2 4 4)[
                              cos(angle)::Double, 0.0, sin(angle)::Double, 0.0,
                              0.0, 1.0, 0.0, 0.0,
                              (0.0-sin(angle))::Double, 0.0, cos(angle)::Double, 0.0,
                              0.0, 0.0, 0.0, 1.0]



rotMatZ :: Double -> R.Array R.U R.DIM2 Double
rotMatZ angle =(R.fromListUnboxed (R.ix2 4 4) [cos(angle)::Double, (0.0-sin(angle))::Double, 0.0, 0.0,
                                              sin(angle)::Double,cos(angle)::Double, 0.0, 0.0,
                                              0.0, 0.0, 1.0, 0.0,
                                              0.0, 0.0, 0.0, 1.0])

                                              
                                              
testRandVec::DoubleVector ->  Double -> StdGen -> IO ()
testRandVec v frustum gen = do
                                let gen'=snd (next gen)
                                
                                temp <- randVec v frustum gen'
                                
                                let temp2=vLength temp
                                
                                putStrLn $ show (temp) ++"   " ++ show(temp2)
                                testRandVec v frustum gen'
                                               
randVec:: DoubleVector ->  Double -> StdGen -> IO DoubleVector
randVec v spawnFrustum randG = do
                    --randG <- getStdGen
                    --calculate the length of "Normal" in the XY-plane
                    let lengthxy = (v R.! (R.Z R.:. 0)) * (v R.! (R.Z R.:. 0)) + (v R.! (R.Z R.:. 1)) * (v R.! (R.Z R.:. 1)) -- sqared length                    
                    --let alphaxy = 0
                    alphaxy <- alphaxyAng v lengthxy
                   
                    --calculate the length of "Normal" in the XZ-plane
                    let lengthxz = (v R.! (R.Z R.:. 0)) * (v R.! (R.Z R.:. 0)) + (v R.! (R.Z R.:. 2)) * (v R.! (R.Z R.:. 2)) -- sqared length
                    alphaxz <- alphaxzAng v lengthxz
                    



                    let vector_x = (R.fromListUnboxed (R.ix2 1 4) [1,0,0,1])            
                    -- pick angle and make rotation matrix
                    let (rand',randG2) = randomR (-1000,1000) randG -- <- undefined
                    let rand = (rand'/1000)::Double
                    let tempRand = (rand * spawnFrustum) - (spawnFrustum/2)


                     --pick a new angle and make another rotation matrix
                    -- rot z
                    let mat = rotMatZ tempRand

                    vector_x2 <- mmultP vector_x mat
                    let (rand2',_) = randomR (-1000,1000) randG2 -- <- undefined
                    let rand2 = rand2'/(1000.0)
                    let tempRand2 = rand2 * (2::Double) * (3.14::Double)
                     -- rot x
                    let mat2 = rotMatX tempRand2

                    vector_x3 <- mmultP vector_x2 mat2

                    let mat3 = rotMatZ alphaxy
                                                
                    vector_x4 <- mmultP vector_x3  mat3;

                    let mat4 = rotMatY alphaxz
                    vector_x5 <- mmultP vector_x4 mat4
                    
                    return $ R.fromListUnboxed (R.ix1 3) [ (vector_x5 R.! (R.Z R.:. 0 R.:. 0))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3)) ,(vector_x5 R.! (R.Z R.:. 0 R.:. 1))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3)),(vector_x5 R.! (R.Z R.:. 0 R.:. 2))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3))] 
    where 
        alphaxyAng:: DoubleVector -> Double -> IO Double
        alphaxyAng v a = case a == 0 of
                True -> return (0.0::Double)
                False -> do 
                           let lengthxy2 = sqrt(a) -- real length
                           --calculate the angle between the x-axis and "Normal" in the XY-plane
                           let alphaxy = asin(((v R.! (R.Z R.:. 1)) / lengthxy2))
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
    
<<<<<<< HEAD
    let yzRotNormOx = R.fromListUnboxed (R.ix1 3) [(v R.! (R.Z R.:. 0)),0,0]
    let yzRotNormOy = R.fromListUnboxed (R.ix1 3) [0,(v R.! (R.Z R.:. 1)),0]
    let yzRotNormOz = R.fromListUnboxed (R.ix1 3) [0,0,(v R.! (R.Z R.:. 2))]
    x <- (dotProd yzRotNormOx  (R.fromListUnboxed (R.ix1 3) [1,0,0]))
    y <- (dotProd yzRotNormOy  (zeroVec))
    z <- (dotProd yzRotNormOz  (zeroVec))
    let xAngle = acos x
    let yAngle = acos y
    let zAngle = acos z
    rotYyzRotNorm <- mmultP xyzRotNorm (R.fromListUnboxed (R.ix2 3 3) [cos yAngle, 0, sin yAngle
=======
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
>>>>>>> ea186f519461cf822cf79c874663dc07456aa21f
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
            