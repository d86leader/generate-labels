{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
module Main where

import Data.OverloadedLabels.TH (generateLabels, generateLabelsUnderscore)
import GHC.OverloadedLabels (IsLabel (fromLabel))

data Foo = Foo {field :: Integer, what :: String}
data Bar = Bar {field :: Integer, that :: String}
data Baz = Baz {_field :: String}

data Poly a = Poly { unPoly :: a }
data ManyPoly a b = ManyPoly
    { field :: a
    , another :: (b, Int)
    }

generateLabels ''Foo
generateLabels ''Bar
generateLabelsUnderscore ''Baz
generateLabels ''Poly
generateLabels ''ManyPoly


main :: IO ()
main = let foo = Foo {field = 1, what = "yes"}
           bar = Bar {field = 10, that = ", I do"}
           -- underscore
           baz = Baz "works too"
           -- polymorphic
           poly = Poly ("hello" :: String)
           -- many polymorphic variables
           manyPoly = ManyPoly (5 :: Int) ("pain" :: String, 10)
           -- monomorphising on access
           x = #field foo :: Integer
           y = #field bar :: Integer
           -- i can monomorphize only one variable, and by it's usage with
           -- another, monomorphization occurs
           five = #field manyPoly :: Int
           (message, val) = #another manyPoly
       in print x
       >> print y
       -- only works with putStrLn monomorphizing it, doesn't work with print
       >> putStrLn (#what foo <> #that bar)
       -- same
       >> putStrLn (#field baz)
       -- same, but this is reaally polymorphic, so
       >> putStrLn (#unPoly poly)
       -- using monos again
       >> putStrLn message
       >> print (five + val)
