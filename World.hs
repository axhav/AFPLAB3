{-# LANGUAGE TypeOperators #-}
module World (
World(..),Color, Object(..), Shape (..),calcNormal,
Light(..)
) where
import qualified Data.Array.Repa as R
import Vector
import Data.Word

type Color =  (Word8,Word8,Word8)--R.Array R.U (R.Z R.:. Int) Word8
data World = World {
    items :: [Object]
    ,lights :: [Light]
    }
    

data Object = Object {
                    shape :: Shape
                    ,color :: Color
                    ,reflectance :: Double
                    }
    deriving (Show)

data Shape = 
        Sphere {
        spos :: DoubleVector
        ,radius :: Double
        } 
        | Plane {
        ppos :: DoubleVector
        ,pnormal :: DoubleVector
        }
    deriving (Show)

data Light = Light{
    lpos :: DoubleVector
    ,lcolor:: Color
    }
    
    
calcNormal :: Object -> DoubleVector -> DoubleVector
calcNormal o@Object{shape =s@Sphere{spos = pos}} pnt = sphereNormal s pnt
calcNormal o@Object{shape =s@Plane{pnormal = norm}} _ = norm 
    
sphereNormal :: Shape -> DoubleVector -> DoubleVector
sphereNormal s@Sphere{spos= pos} pnt = normalize $ R.computeUnboxedS $ R.zipWith (-) pos pnt



