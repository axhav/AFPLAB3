{-# LANGUAGE TypeOperators #-}
module World (
World(..),Color, Object(..), Shape (..),calcNormal,
Light(..),cmul,colRound,cadd
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
sphereNormal s@Sphere{spos= pos} pnt = normalize $ R.computeUnboxedS $ R.zipWith (-)  pnt pos

cadd :: Color -> Color -> Color
cadd (r1,g1,b1) (r2,g2,b2) = (test r1 r2, test g1 g2, test b1 b2)
    where test a b | ((fromIntegral a) + (fromIntegral b)) >= 255.0 = 255
                   | otherwise = a+b

cmul :: Color -> Double -> Color
cmul (r,g,b) d = (colRound((fromIntegral r)*d),colRound((fromIntegral g)*d) ,colRound((fromIntegral b)*d))

colRound :: Double -> Word8
colRound d | d >= 255.0 = 255
           | d <= 0.0 = 0
           | otherwise = round d