{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE InstanceSigs #-}

module Lib
    ( someFunc
    ) where

import Data.Monoid ((<>))
import Control.Monad.Except (MonadError, throwError)

import qualified Data.Bson              as Bson
import Data.Bson (Field ((:=)), val)
import           Data.Map               (Map)
import qualified Data.Map as Map
import           Data.Proxy             (Proxy (Proxy))
import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Control.Monad.IO.Class (MonadIO, liftIO)

-- mongo backend

import qualified Database.MongoDB       as Mongo
import qualified Database.MongoDB.Query as Mongo

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- cellstore mongodb backend

type DbHost = Text
type DbName = Text
type DbCollection = Text

mkCellStoreMongoDb :: MonadIO m
                  => DbName
                  -> DbCollection
                  -> DbHost
                  -> Mongo.AccessMode
                  -> m (CellStore MongoBackend)
mkCellStoreMongoDb name collection host accessMode = do
    pipe <- liftIO $ Mongo.connect $ Mongo.host (Text.unpack host)
    pure $ CellStore MongoBackend
      { mbPipe       = pipe
      , mbDbName     = name
      , mbDbCollection = collection
      , mbAccessMode = accessMode
      }

data MongoBackend = MongoBackend
  { mbPipe       :: Mongo.Pipe
  , mbDbName     :: DbName
  , mbDbCollection :: DbCollection
  , mbAccessMode :: Mongo.AccessMode
  }

class CellStoreBackend b where
  type DbCellType b :: *
  type DbValueType b :: *
  save :: MonadIO m => b -> Map Dimension DimValue -> DbValueType b -> m ()
  load :: (MonadIO m, MonadError Text m)
       => b -> Map Dimension DimValue -> m (DbValueType b)

instance CellStoreBackend MongoBackend where
  type DbCellType MongoBackend = Bson.Document
  type DbValueType MongoBackend = Bson.Value
  save MongoBackend{..} dims value =
      () <$ Mongo.access mbPipe mbAccessMode mbDbName
          (Mongo.insert mbDbCollection $ ("value" := value) : dimsToDoc dims)
  load MongoBackend{..} dims = do
      doc <- Mongo.access mbPipe mbAccessMode mbDbName $
          Mongo.findOne (Mongo.select (dimsToDoc dims) mbDbCollection)
      maybe (throwError "Not found") (Mongo.look "value") doc

dimsToDoc :: Map Dimension DimValue -> Bson.Document
dimsToDoc dims = foldl acc [] (Map.toList dims)
  where
    acc ds (dim, dimval) = ("dim" <> dim := val dimval) : ds

-- cellstore interface

data CellStore a where
  CellStore :: CellStoreBackend a => a -> CellStore a

class DataPointClass a where
    dimensions :: a -> Map Dimension DimValue

class (CellStoreBackend (Backend a), DataPointClass (DataPoint a)) => Cell a where
    type DataPoint a :: *
    type Backend a :: *
    mkValue :: a -> DbValueType (Backend a)

saveCell :: (MonadIO m, Cell a)
         => CellStore (Backend a) -> a -> DataPoint a -> (Dimension, DimValue) -> m ()
saveCell (CellStore backend) c d (specificDim, specificDimVal) =
    save backend (Map.insert specificDim specificDimVal $ dimensions d) (mkValue c)

loadCell :: (MonadIO m, MonadError Text m, Cell a)
         => CellStore (Backend a)
         -> a
         -> DataPoint a
         -> (Dimension, DimValue)
         -> m (DbValueType (Backend a))
loadCell (CellStore backend) c d (specificDim, specificDimVal) =
  load backend (Map.insert specificDim specificDimVal $ dimensions d)

-- cellstore types

type Dimension = Text
type DimValue = Text

-- instances

data ItemDataPoint

instance DataPointClass ItemDataPoint where
    dimensions _ = [("BASE", "BAS")]

newtype Item = Item { unItem :: Double }

instance Cell Item where
    type Backend Item = MongoBackend
    type DataPoint Item = ItemDataPoint
    mkValue (Item d) = val d
