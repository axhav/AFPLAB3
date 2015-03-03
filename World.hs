{-# LANGUAGE TypeOperators #-}
module World (
World, Object, Sphere (..)

) where
import qualified Data.Array.Repa as R

type DoubleVector = R.Array R.U (R.Z R.:. Int) Double

type World = [Object]

type Object =  Sphere


data Sphere = Sphere {
        position :: DoubleVector
        ,radius :: Double
        }
        

