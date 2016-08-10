{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import qualified Database.MongoDB       as Mongo
import Control.Monad.Except (runExceptT)
import Data.Proxy (Proxy (Proxy))

import Lib

main :: IO ()
main = do
    cs <- mkCellStoreMongoDb "test" "cellstore" "127.0.0.1" Mongo.master
    let myItem = Item 0.444
    saveCell cs myItem (ItemContext Proxy) ("Foo", "foo")
    eItem <- runExceptT $ loadCell cs (Proxy :: Proxy Item) (ItemContext Proxy) ("Foo", "goo")
    print eItem
    pure ()
