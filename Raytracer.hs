{-# LANGUAGE TypeOperators #-}

import qualified Data.Array.Repa as R
import Ray
import World

dummyRay :: Ray
dummyRay = Ray { dir =  R.fromListUnboxed (R.ix1 4) [1,0,0,0] 
                ,point = R.fromListUnboxed (R.ix1 3) [0,0,0]
               }
                
dummySphere :: Object
dummySphere = Sphere {
                position =  R.fromListUnboxed (R.ix1 4) [-10,0,0,0] 
                ,radius = 1.0
            }
--                 
trace :: Ray -> Depth -> IO Color
trace r@Ray{dir = dir, point = pnt} d = do
    case d of
        5 -> return $ R.fromListUnboxed (R.ix1 4) [0,0,0,0]     -- byt 5an till dynamisk?
        _ ->undefined