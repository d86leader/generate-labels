{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.OverloadedLabels.TH (generateLabels)

data Foo = Foo {field :: Integer, what :: String}
data Bar = Bar {field :: Integer, that :: String}

generateLabels ''Foo
generateLabels ''Bar


main :: IO ()
main = let foo = Foo {field = 1, what = "yes"}
           bar = Bar {field = 10, that = ", I do"}
           x = #field foo :: Integer
           y = #field bar :: Integer
       in print x
       >> print y
       >> print (x + y)
       >> putStrLn (#what foo <> #that bar)
