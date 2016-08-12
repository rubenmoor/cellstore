{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import qualified Database.MongoDB       as Mongo
import Control.Monad.Except (runExceptT)
import Data.Proxy (Proxy (Proxy))

import Lib

lss :: [[Int]]
lss = [[1,2,3], [10,20], [100]]

combine :: [[Int]] -> [[Int]]
combine (l:ls) = mapExplode l $ combine ls
combine []     = [[]]

mapExplode :: [a] -> [[a]] -> [[a]]
mapExplode xs (y:ys) = append (explode xs y) (mapExplode xs ys)
mapExplode _  []     = []

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

explode :: [a] -> [a] -> [[a]]
explode (x:xs) y = (x:y) : explode xs y
explode []     _ = []

main :: IO ()
main = do
    cs <- mkCellStoreMongoDb "test" "cellstore" "127.0.0.1" Mongo.master
    let myItem = Item 0.444
    saveCell cs myItem (ItemContext Proxy) ("Foo", "foo")
    eItem <- runExceptT $ loadCell cs (Proxy :: Proxy Item) (ItemContext Proxy) ("Foo", "goo")
    print eItem
    print $ combine lss
    pure ()
