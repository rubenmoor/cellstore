{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module TypeClassApproach where

import           GHC.TypeLits (KnownSymbol, symbolVal)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Database.MongoDB       as Mongo
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Kind (Type)
import           Data.Monoid ((<>))
import           Control.Monad.Except (MonadError, throwError)
import qualified Data.Map as Map
import qualified Data.Bson as Bson
import           Data.Bson (Field ((:=)))
import           Data.Proxy (Proxy (Proxy))

import           Types
import           TypeFunctions

-- cellstore mongodb backend

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
  type DbCellType b :: Type
  type DbValueType b :: Type
  save :: MonadIO m => b -> Map.Map Dimension DimValue -> DbValueType b -> m ()
  load :: (MonadIO m, MonadError Text m)
       => b -> Map.Map Dimension DimValue -> m (DbValueType b)

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

dimsToDoc :: Map.Map Dimension DimValue -> Bson.Document
dimsToDoc dims = foldl acc [] (Map.toList dims)
  where
    acc ds (dim, dimval) = ("dim" <> dim := Bson.val dimval) : ds

-- cellstore interface

data CellStore a where
  CellStore :: CellStoreBackend a => a -> CellStore a

class (CellStoreBackend (Backend a), DataPointClass (DataPoint a)) => Cell a where
    type DataPoint a :: Type
    type Backend a :: Type
    mkValue :: a -> DbValueType (Backend a)

class DataPointClass a where
    context :: a -> Map.Map Dimension DimValue

saveCell :: (MonadIO m, Cell a)
         => CellStore (Backend a) -> a -> DataPoint a -> (Dimension, DimValue) -> m ()
saveCell (CellStore backend) c d (specificDim, specificDimVal) =
    save backend (Map.insert specificDim specificDimVal $ context d) (mkValue c)

loadCell :: (MonadIO m, MonadError Text m, Cell a)
         => CellStore (Backend a)
         -> Proxy a
         -> DataPoint a
         -> (Dimension, DimValue)
         -> m (DbValueType (Backend a))
loadCell (CellStore backend) Proxy d (specificDim, specificDimVal) =
  load backend (Map.insert specificDim specificDimVal $ context d)

-- type-level function

class Dimensions a where
    dimensions :: Proxy a -> [(Dimension, DimValue)]

instance Dimensions '[] where
    dimensions _ = []

instance (KnownSymbol d, KnownSymbol v, Dimensions ds) => Dimensions ( '(d, v) ': ds ) where
  dimensions _ =
      (symbolText (Proxy :: Proxy d),
       symbolText (Proxy :: Proxy v)) : dimensions (Proxy :: Proxy ds)
    where
      symbolText :: KnownSymbol a => Proxy a -> Text
      symbolText = Text.pack . symbolVal

-- instances

newtype ItemContext = ItemContext (Proxy '[ '("BASE", "BAS") ])

instance DataPointClass ItemContext where
    context (ItemContext p) = Map.fromList $ dimensions p

newtype Item = Item { unItem :: Double }

instance Cell Item where
    type Backend Item = MongoBackend
    type DataPoint Item = ItemContext
    mkValue (Item d) = Bson.val d

-- cellstore model

type Model = Item   $|$ Aspect "Foo" '["bar"] |$ Aspect "Cus" '["eur", "usd"] |$ Aspect "x" '["y"]
         &&| Double $|$ Aspect "Foo" '["bar", "baz"]
         &&| Int    $|$ Aspect "Foo" '["boz"]
