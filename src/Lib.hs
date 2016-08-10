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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Compose (Flip)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Monoid ((<>))
import Control.Monad.Except (MonadError, throwError)
import Data.Kind (Type)
import Data.Type.List (Difference)

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

type SimpleModel = Integer $|$ Aspect "foo" "bar" |$ Aspect "egon" "hugo"
               &&| Double  $|$ Aspect "foo" "baz"

-- there is no query to get cells that have different aspects,
--   i.e. queries run along dimensional values for a given set of dimensions
-- a data point model with cells that differ by the absence of one aspect
--   is discouraged, though it shouldn't lead to problems
-- redundant definition of a data type (e.g. twice the same data type
--    with same aspects but other
--    dimensional values) is nonsensical and should yield a compile error,
--    ideally
-- the data point null (no aspects) currently cannot be used

-- list query: horizontal query results in list
--             vertical queries result in tuples of (Maybe a)
-- singleton query: always result in (Maybe a)

data (&&|) a b
infixr 1 &&|

data ($|$) a b
infixr 2 $|$

data (|$) a b
infixr 3 |$

data Aspect a b

-- type-level errors

data MalformedModel
data MalformedQuery
data QueryNoMatch

-- type function

-- | given a database model and a singleton query, return the query result type
--
-- singleton query example
-- > type Query = Aspect "Foo" "bar" |$ Aspect "Cus" "eur"
--

type family CellTypeSingle (model :: Type) (query :: Type) :: Type where
    CellTypeSingle m q = CellTypeSingleUnwrap (CellTypeSingle' m q)

type family CellTypeSingleUnwrap (a :: Maybe Type) :: Type where
    CellTypeSingleUnwrap ('Just a) = a
    CellTypeSingleUnwrap 'Nothing  = QueryNoMatch

type family CellTypeSingle' (model :: Type) (query :: Type) :: Maybe Type where
    CellTypeSingle' (v $|$ as) q =
        IfThenElse (Null (Difference (OpList as) (OpList q))) ('Just v) 'Nothing
    CellTypeSingle' c q = FindJust (MapCellTypeSingle' q (OpList c))

-- | turn aspects connected with '|$' and cells connected with '&&|' into a list
-- of aspects or cells, respectively
type family OpList aspects :: [Type] where
    OpList (op x xs) = x ': OpList xs
    OpList x         = '[x]

type family Null ls :: Bool where
    Null '[] = 'True
    Null a   = 'False

type family IfThenElse (cond :: Bool) a b where
    IfThenElse 'True  x y = x
    IfThenElse 'False x y = y

type family FindJust ls :: Maybe Type where
    FindJust '[] = 'Nothing
    FindJust ('Just a  ': _)  = 'Just a
    FindJust ('Nothing ': xs) = FindJust xs

type family MapCellTypeSingle' (query :: Type) (cells :: [Type]) :: [Maybe Type] where
    MapCellTypeSingle' q (c ': cs) = CellTypeSingle' c q ': MapCellTypeSingle' q cs
    MapCellTypeSingle' _ '[]       = '[]

-- | given a database model and a list query, return the query result type
-- type family   CellTypeList :: Type -> Type -> Type
-- type instance CellTypeList (v $|$ a) q = Map (CellTypeSingle (v $|$ a)) (RolloutQuery q)

type family RolloutQuery aspect :: [Type] where
    RolloutQuery (Aspect d (v ': vs)) = Aspect d v : RolloutQuery (Aspect d vs)
    RolloutQuery (Aspect _ '[])       = '[]

-- Query

type Any = '[]

-- interface

save' :: MonadIO m => Proxy model -> Proxy q -> CellTypeSingle model q -> m ()
save' = undefined

test :: IO ()
test = save' (Proxy :: Proxy SimpleModel)
             (Proxy :: Proxy (Aspect "foo" "baz" |$ Aspect "foo" "bar"))
             3
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
    acc ds (dim, dimval) = ("dim" <> dim := val dimval) : ds

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
