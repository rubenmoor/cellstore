{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Monoid ((<>))
import Control.Monad.Except (MonadError, throwError)
import Data.Kind (Type)
import Data.Type.List (Map, Find)

import qualified Data.Bson              as Bson
import Data.Bson (Field ((:=)), val)
import qualified Data.Map as Map
import           Data.Proxy             (Proxy (Proxy))
import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Control.Monad.IO.Class (MonadIO, liftIO)

-- mongo backend

import qualified Database.MongoDB       as Mongo

-- cellstore model

type Model = Item   $|$ Aspect "Foo" '["bar"] |$ Aspect "Cus" '["eur", "usd"] |$ Aspect "x" '["y"]
         &&| Double $|$ Aspect "Foo" '["bar", "baz"]
         &&| Int    $|$ Aspect "Foo" '["boz"]

-- there is no query to get cells that have different aspects,
--   i.e. queries run along dimensional values for a given set of dimensions
-- a data point model with cells that differ by the absence of one aspect
--   is discouraged, though it shouldn't lead to problems
-- redundant definition of a data type (e.g. twice the same data type
--    with same aspects but other
--    dimensional values) is nonsensical and should yield a compile error,
--    ideally
-- the data point null (no aspects) currently cannot be used

data (&&|) a b

data ($|$) a b

data (|$) a b

data Aspect a b

-- type function

class ClsCellType m where
    type CellType m q :: Type

instance ClsCellType (a $|$ b) where
    type CellType (a $|$ b) q = Map (\x -> Find x b) (RolloutQuery q)
      a
      [a]

type family RolloutQuery q :: [Type]
type instance RolloutQuery (Aspect d (v ': vs)) = Aspect d v ': RolloutQuery (Aspect d vs)
type instance RolloutQuery (Aspect _ '[])       = '[]

-- Query

type Any = '[]

-- interface

save' :: MonadIO m => Proxy m -> Proxy q -> CellType m q -> m ()
save' = undefined

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

dimsToDoc :: Map Dimension DimValue -> Bson.Document
dimsToDoc dims = foldl acc [] (Map.toList dims)
  where
    acc ds (dim, dimval) = ("dim" <> dim := val dimval) : ds

-- cellstore interface

data CellStore a where
  CellStore :: CellStoreBackend a => a -> CellStore a

class (CellStoreBackend (Backend a), DataPointClass (DataPoint a)) => Cell a where
    type DataPoint a :: *
    type Backend a :: *
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

-- cellstore types

type Dimension = Text
type DimValue = Text

-- instances

newtype ItemContext = ItemContext (Proxy '[ '("BASE", "BAS") ])

instance DataPointClass ItemContext where
    context (ItemContext p) = Map.fromList $ dimensions p

newtype Item = Item { unItem :: Double }

instance Cell Item where
    type Backend Item = MongoBackend
    type DataPoint Item = ItemContext
    mkValue (Item d) = val d
