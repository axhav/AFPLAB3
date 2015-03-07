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
                
dummySphere :: Sphere
dummySphere = Sphere {
                position =  R.fromListUnboxed (R.ix1 3) [10,0,0] 
                ,radius = 1.0
            }
            
dummySphere2 :: Sphere
dummySphere2 = Sphere {
                position =  R.fromListUnboxed (R.ix1 3) [5,0,0] 
                ,radius = 2.0
            }

dummyObj = Object{shape =dummySphere2
             , color=(0,255,0) -- (R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0}
           
            
dummyWorld :: World
dummyWorld = [-- Object{shape =dummySphere
             --, color= (255,0,0) --(R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             --,reflectance = 0}
             Object{shape =dummySphere2
             , color=(0,255,0) -- (R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0}]



dummyVec1 :: DoubleVector
dummyVec1 = R.fromListUnboxed (R.ix1 3) [1,0,0,0]


dummyVec2 :: DoubleVector
dummyVec2 = R.fromListUnboxed (R.ix1 4) [20,1,0,0]

cameraRay :: Camera-> Int -> Int -> Ray
cameraRay r@Camera{cdir = dir, cpoint = pnt, cup =u} x y =
     Ray{dir= ( R.computeUnboxedS( R.zipWith (-) imagePoint pnt)), point = pnt}
        where cam_right = crossProd dir u
              maxX = 200
              maxY = 200
              normX = ((fromIntegral x ) /maxX) -0.5
              normY =  ((fromIntegral y ) /maxY) -0.5
              imagePoint = (R.zipWith (+) (R.zipWith (+) (R.zipWith (+) (R.map (normX*) cam_right)
                    (R.map (normY*) u)) pnt) dir )

cameraRay2 :: Camera -> Int -> Int -> Ray
cameraRay2 r@Camera{cdir = dir, cpoint = pnt, cup =up} x y =
     Ray{dir= normalize( R.computeUnboxedS(R.zipWith (-) pnt imagePoint)), point = pnt}  
            where u = crossProd dir up
                  v = crossProd u dir
                  halfWidth = 0.5
                  halfHeight = 0.5
                  viewPlaneHalfWith = tan (90.0/2.0)
                  aspectRatio = 1
                  viewPlaneHalfHeight = viewPlaneHalfWith * aspectRatio
                  viewPlaneBottomLeftPoint = R.zipWith (-) ( R.zipWith (-) (dir) (R.map ( viewPlaneHalfHeight* ) v))  (R.map ( viewPlaneHalfWith* ) u)
                  xIncVector = R.map (*(2*halfWidth/ 200.0)) u
                  yIncVector = R.map (*(2*halfHeight/ 200.0)) v
                  imagePoint = R.zipWith (+) (R.zipWith (+) viewPlaneBottomLeftPoint (R.map ( (fromIntegral x)* ) xIncVector)) (R.map ( ( fromIntegral y)* ) yIncVector)
                  
main :: IO ()
main = do
    let path = "test.bmp"
    let widht = 200
    let height = 200
    putStrLn $ "Starting trace on a " ++ show widht ++ "x" ++ show height ++ " ..."
    let w = dummyWorld 
    let c = dummyCam
    --putStrLn $ show [(cameraRay2 c x y)| x <- [0..(widht-1)], y <- [0..(height-1)]]
    ls <- sequence [ trace w (cameraRay c x y) 0 | x <- [0..(widht-1)], y <- [0..(height-1)]]
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
                    let randInt = next rand 
                    let randInt2 = fst $ next (snd randInt)
                    let norm = sphereNormal (shape obj) hitp
                    newDir <-  calcHemispherePoint (fst randInt) randInt2 norm
                    cos_theta <- dotProd newDir norm
                    let brdf = 2 * (reflectance obj) * cos_theta
                    --putStrLn $ show newDir
                    reflCol <- trace w (Ray{dir=newDir, point = hitp}) (d+1)                    
                    return $ calcFinalCol emittance reflCol brdf
                    
                 
            
calcFinalCol :: Color -> Color -> Double ->Color
calcFinalCol (er,eg,eb) (rr,rg,rb) brf = (calc er rr,calc eg rg,calc eb rb)  --R.computeUnboxedS$ R.map round $ R.map (b*) $ R.map fromIntegral rc
    where calc a b = a + round (brf * fromIntegral b)

    
calcHemispherePoint :: Int -> Int -> DoubleVector -> IO DoubleVector
calcHemispherePoint a b v = randVec v (toRad a) (toRad b) (toRad b) 
    where toRad deg = 2.0*3.14 *( (fromIntegral deg)/180.0)
            
calcNorm =undefined 



randVec:: DoubleVector -> Double -> Double ->  Double -> IO DoubleVector
randVec v r1 r2 r3 = do
    yRot <- mmultP  (R.fromListUnboxed (R.ix2 1 3) [1,0,0]) (R.fromListUnboxed (R.ix2 3 3) [cos r2, 0, sin r2
                                                            ,0,1,0
                                                            ,-sin r2,0,cos r2]) 
    zRot <- mmultP yRot (R.fromListUnboxed (R.ix2 3 3) [cos r3, -sin r3, 0
                                                        ,sin r3,cos r3,0
                                                        ,0,0,0]) 
    
    xyzRotNorm  <- mmultP zRot (R.fromListUnboxed (R.ix2 3 3) [1,0,0
                                                             ,0,cos r1,-sin r1
                                                             ,0,sin r1,cos r1])
    
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
                                                        ,0,1,0
                                                        ,-sin yAngle,0,cos yAngle])
    rotYZyzRotNorm <-  mmultP rotYyzRotNorm (R.fromListUnboxed (R.ix2 3 3) [cos zAngle, -sin zAngle, 0
                                                          ,sin zAngle,cos zAngle,0
                                                          ,0,0,0])
    rotYZXyzRotNorm <- mmultP rotYZyzRotNorm (R.fromListUnboxed (R.ix2 3 3) [1,0,0
                                                     ,0,cos xAngle,-sin xAngle
                                                     ,0,sin xAngle,cos xAngle])
                                                    
    
                                                     
    return $ R.fromListUnboxed (R.ix1 3) [(rotYZXyzRotNorm R.! (R.Z R.:. 0 R.:. 0)) ,(rotYZXyzRotNorm R.! (R.Z R.:. 0 R.:. 1)),(rotYZXyzRotNorm R.! (R.Z R.:. 0 R.:. 2))]                                        
        where
            zeroVec = R.fromListUnboxed (R.ix1 3) [0,0,0]

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
            