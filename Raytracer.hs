{-# LANGUAGE TypeOperators #-}
import Control.Monad.State
import Control.Applicative
import System.IO.Unsafe
import qualified Data.Array.Repa                  as R
import qualified Data.Array.Repa.Eval             as R
import qualified Data.Array.Repa.Unsafe           as R
import Data.Array.Repa.IO.BMP
import System.Random
import Data.Word

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
                spos =  R.fromListUnboxed (R.ix1 3) [10,2,0] 
                ,radius = 4.0
            }
            
dummySphere2 :: Shape
dummySphere2 = Sphere {
                spos =  R.fromListUnboxed (R.ix1 3) [5,-3,-3] 
                ,radius = 2.0
            }

dummyPlane :: Shape
dummyPlane = Plane {
                ppos =  R.fromListUnboxed (R.ix1 3) [0,-1,0] 
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
dummyWorld = World{items =[Object{shape =dummyPlane
             , color= (150,0,0) --(R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 1000},
             Object{shape =dummySphere
             , color=(0,0,255) -- (R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0}]
             ,lights = [Light{ 
                lpos =  R.fromListUnboxed (R.ix1 3) [0,10,0]
                ,lcolor = (255,255,255)
             }]
             } {-
             Object{shape =dummyPlane2
             , color=(0,150,0) -- (R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 1000},
             ] -}



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
                  halfWidth = 400
                  halfHeight = 400
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
                  pixelDir = (R.zipWith (+) (R.zipWith (+) (R.map (dist*) dir) (R.map ((0.5 - (fromIntegral y)/( fromIntegral(maxY -1)))*) cr) ) (R.map ((0.5 - (fromIntegral x)/( fromIntegral(maxX -1)))*) cu))
                  
main :: IO ()
main = do
    let path = "test.bmp"
    let widht = 200
    let height = 200
    putStrLn $ "Starting trace on a " ++ show widht ++ "x" ++ show height ++ " ..."
    let w = dummyWorld 
    let c = dummyCam
    let indexs = [(0,0,0)| x<- [0..(widht-1)], y <- [0..(height-1)] ]
    --putStrLn $ show [(cameraRay2 c x y)| x <- [0..(widht-1)], y <- [0..(height-1)]]
    let image = R.fromListUnboxed (R.ix2 widht height) indexs
    let final = R.computeUnboxedS $ R.traverse image id (\f x -> traceFunc f x widht height w c) -- () -- (\_ (R.Z R.:. ax R.:. ay R.:. _ ) ->(R.Z R.:. ax R.:. ay R.:. ))
    putStrLn $ "Trace is done creating image named " ++ show path
    writeImageToBMP ("./"++path) final
    {-
    ls <- sequence [trace w (cameraRay c (widht,height) x y) 0 | x <- [0..(widht-1)], y <- [0..(height-1)]]
    putStrLn $ "Trace is done creating image named " ++ show path
    let image = R.fromListUnboxed (R.ix2 widht height) ls
    writeImageToBMP ("./"++path) image
-}
--      
traceFunc :: (R.DIM2 -> Color) -> R.DIM2 -> Int -> Int -> World -> Camera -> Color --(Word8
traceFunc _ (R.Z R.:. ax R.:. ay) widht height w c = unsafePerformIO(trace w (cameraRay (c) (widht,height) ax ay) 0)
           
trace :: World -> Ray -> Depth -> IO Color
trace w r@Ray{dir = dir, point = pnt} d = do
    case d of
        3 ->do
            return $ (0,0,0)     -- byt 5an till dynamisk?
        _ -> do 
            test <-intersectWorld r w 
            case test of
                Nothing -> return $ (0,0,0)
                (Just (obj,hitp)) -> do
                    --putStrLn $ "hit"
                    lintens <- intersectLights hitp w
                    let emittance = color obj
                    rand <- getStdGen
                    let norm = calcNormal obj hitp
                    newDir <- getSampledBiased norm 0 rand ---randVec norm 3.14 rand 
                    cos_theta <- dotProd (normalize newDir) norm
                    let brdf = 2 * (reflectance obj) * cos_theta
                    --putStrLn $ show newDir
                    reflCol <- trace w (Ray{dir=newDir, point = hitp}) (d+1)                    
                    return $ calcFinalCol emittance reflCol brdf lintens
                    
                 
            
calcFinalCol :: Color -> Color -> Double ->Double ->Color
calcFinalCol (er,eg,eb) (rr,rg,rb) brf light = (calc er rr light,calc eg rg light,calc eb rb light)  --R.computeUnboxedS$ R.map round $ R.map (b*) $ R.map fromIntegral rc
    where calc a b l =  (round ((fromIntegral a)*0.1)) + ((a  + round (brf * fromIntegral b))*round l)

    
getSampledBiased :: DoubleVector -> Double -> StdGen -> IO DoubleVector
getSampledBiased dir pow randG = do
    let dir' = normalize dir
    let o1 = normalize $ ortho dir
    let o2 = normalize $ crossProd dir o1
    let (rand',randG2) = randomR (-1000,1000) randG -- <- undefined
    let rand = (rand'/1000)
    let (rand2',randG3) = randomR (-1000,1000) randG2 -- <- undefined
    let rand2 = (rand2'/1000)
    let randV =  R.fromListUnboxed (R.ix1 2) [rand,rand2]
    let randV2 = R.fromListUnboxed (R.ix1 2) [(randV R.! (R.Z R.:. 0))*2.0*pi, (randV R.! (R.Z R.:. 0))**(1.0/(pow+1.0))]
    let onemius = sqrt (1.0 - ((randV2 R.! (R.Z R.:. 1))*(randV2 R.! (R.Z R.:. 1))))
    return $ R.computeUnboxedS $ R.zipWith (+) ( R.zipWith (+) (R.map ((cos (randV2 R.! (R.Z R.:. 0)) * onemius)*) o1)
        (R.map ((sin (randV2 R.! (R.Z R.:. 0)) * onemius)*) o2)) (R.map (randV2 R.! (R.Z R.:. 0)*) dir)
    
    
    {-
rotMatX :: Double -> R.Array R.U R.DIM2 Double
rotMatX angle =(R.fromListUnboxed (R.ix2 4 4) [
                                               1.0, 0.0        ,0.0             ,0.0,                                                
                                               0.0, cos(angle)::Double, (0.0-sin(angle))::Double,0.0,
                                               0.0, sin(angle)::Double, cos(angle)::Double  ,0.0,
                                               0, 0.0         , 0.0           ,1.0
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
                                              0.0               ,0.0               ,1.0 , 0.0,
                                              0.0               , 0.0              , 0.0, 1.0])

                                              
                                              
testRandVec::DoubleVector ->  Double -> StdGen -> IO ()
testRandVec v frustum gen = do
                                let gen'=snd (next gen)
                                
                                temp <- randVec v frustum gen'
                                
                                let temp2=vLength temp
                                
                                --putStrLn $ show (temp) ++"   " ++ show(temp2)
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
                    
                    --putStrLn $ show (alphaxz *180.0/pi)
                    --putStrLn $ show (alphaxy * 180.0/pi)
                    

                    let vector_x = (R.fromListUnboxed (R.ix2 1 4) [1,0,0,1])            
                    -- pick angle and make rotation matrix
                    let (rand',randG2) = randomR (-1000,1000) randG
                    let rand = (rand'/1000)::Double
                    let tempRand = (rand * spawnFrustum) - (spawnFrustum/2.0)::Double


                    --pick a new angle and make another rotation matrix
                    -- rot z
                    let mat = rotMatZ tempRand

                    vector_x2 <- mmultP vector_x mat
                    let (rand2',_) = randomR (0,1000) randG2
                    let rand2 = (rand2'/(1000.0))::Double
                    let tempRand2 = (rand2 * (2::Double) * (pi::Double))-(pi::Double)
                     -- rot x
                    let mat2 = rotMatX tempRand2

                    vector_x3 <- mmultP vector_x2 mat2
                    
                    mat4 <-mmultP (rotMatY alphaxz) (rotMatZ (0.0-alphaxy))     

<<<<<<< HEAD
                    let mat3 = rotMatZ alphaxz
                    
                    
                    vector_x4 <- mmultP vector_x3  mat3

                    let mat4 = rotMatY  alphaxy
                    vector_x5 <- mmultP vector_x4 mat4
                    putStrLn $ show vector_x4
                    putStrLn $ show vector_x5
                    return $ R.fromListUnboxed (R.ix1 3) [ (vector_x5 R.! (R.Z R.:. 0 R.:. 0))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3)) ,0.0-((vector_x5 R.! (R.Z R.:. 0 R.:. 1))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3))),(vector_x5 R.! (R.Z R.:. 0 R.:. 2))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3))] 
=======
                    --vector_x4 <- mmultP vector_x3  mat3;

                    vector_x5 <- mmultP vector_x3 mat4
                    putStrLn $  (show vector_x5)                      
                    return $ R.fromListUnboxed (R.ix1 3) [ (vector_x5 R.! (R.Z R.:. 0 R.:. 0))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3)) ,((vector_x5 R.! (R.Z R.:. 0 R.:. 1))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3))),(vector_x5 R.! (R.Z R.:. 0 R.:. 2))/(vector_x5 R.! (R.Z R.:. 0 R.:. 3))] 
>>>>>>> 1b818a2e8640e40f37e88fdf8fd5e212b8711b72
    where 
        alphaxyAng:: DoubleVector -> Double -> IO Double
        alphaxyAng v a = case a == 0 of
                True -> return (0.0::Double)
                False -> do 
                           let lengthxy2 = sqrt(a) -- real length
                           --calculate the angle between the x-axis and "Normal" in the XY-plane
                           let r = asin(((v R.! (R.Z R.:. 1)) / lengthxy2)::Double)
                           return r
        alphaxzAng:: DoubleVector -> Double -> IO Double
        alphaxzAng v a =
                   case a == 0 of
                        True -> return (0.0) -- 90 degrees
                            
                        False ->  do
                            let lengthxz = sqrt(a) -- real length
                            -- calculate the angle between the x-axis and "Normal" in the XZ-plane
                            let r1 = asin(((v R.! (R.Z R.:. 2)) / lengthxz)::Double) -- sin alpha = z / hypotenuse
                            return r1
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
            -}
