module Main where


import Raytracer
import Data.Time


dummyCam :: Camera
dummyCam = createCamera (1,0,0) (0,0,0) (0,1,0) (45)
              

main :: IO ()
main = do
    putStrLn $ "Starting trace on a " ++ show 100 ++ "x" ++ show 100 ++ " ..."
    t0 <- getCurrentTime
    (trace2BMP obj dummyCam (100,200) 2 "test.bmp")
    t1 <- getCurrentTime
    putStrLn $ "Trace is done (in "++ show (diffUTCTime t1 t0) ++") creating image named " ++ "test.bmp"
    where
       obj = [EntO (createPlane vUp vDown (0.0,150.0,0.0) 1),EntO (createSphere 2.0 (10.0,0.0,0.0) (t2c Blue) 0 ),EntL (createLight (6.0,0.0,0.0) (t2c White))]
    