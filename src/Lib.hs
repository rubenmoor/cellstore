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

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Monoid ((<>))
import Control.Monad.Except (MonadError, throwError)
import Data.Kind (Type)
import Data.Type.List (Difference, Map)

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

type SimpleModel = Integer $|$ Aspect "foo" '["bor"] |$ Aspect "egon" '["hugo"] |$ Nil
               &&| Double  $|$ Aspect "foo" '["baz", "beez"] |$ Aspect "egon" '["balders"] |$ Nil
               &&| Nil

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
infixr 0 &&|

data ($|$) a b
infixr 1 $|$

data (|$) a b
infixr 2 |$

data Nil

data Aspect a b

-- type-level errors

data MalformedModel
data QueryNoMatch

data Match a
  = NoMatch
  | Matched a
  | MatchMalformedModel

-- type function

-- | given a database model and a singleton query, return the query result type
--
-- singleton query example
-- > type Query = Aspect "Foo" "bar" |$ Aspect "Cus" "eur"
--

type family CellTypeSingle (model :: Type) (query :: Type) :: Type where
    CellTypeSingle m q = MatchUnwrap (CellTypeMatch m q)

type family MatchUnwrap (a :: Match Type) :: Type where
    MatchUnwrap ('Matched a)         = a
    MatchUnwrap 'NoMatch             = QueryNoMatch
    MatchUnwrap 'MatchMalformedModel = MalformedModel

type family CellTypeMatch (model :: Type) (query :: Type) :: Match Type where
    CellTypeMatch cs q = FindOne (MapCellTypeMatch q (MapExplodeCube (CubeList cs))) 'Nothing

type family MapCellTypeMatch (query :: Type) (cubes :: [Type]) :: [Maybe Type] where
    MapCellTypeMatch q ((v $|$ as) ': cs) =
      ( IfThenElse (Null (Difference as (AspectList q)))
                   ('Just v) 'Nothing
      ) ': MapCellTypeMatch q cs
    MapCellTypeMatch _ '[]       = '[]

type family FindOne (ls :: [Maybe Type]) (found :: Maybe Type) :: Match Type where
    FindOne '[]              'Nothing  = 'NoMatch
    FindOne '[]              ('Just a) = 'Matched a
    FindOne ('Just a  ': xs) 'Nothing  = FindOne xs ('Just a)
    FindOne ('Just a  ': _)  ('Just b) = 'MatchMalformedModel
    FindOne ('Nothing ': xs) found     = FindOne xs found

-- explode cube

type family MapExplodeCube (cubes :: [Type]) :: [Type] where
    MapExplodeCube ((v $|$ as) ': cs) =
      Append (MapToCube v (CombineCube (ExplodeCube (AspectList as))))
             (MapExplodeCube cs)
    MapExplodeCube '[] = '[]

type family MapToCube (value :: Type) (aspects :: [[Type]]) :: [Type] where
    MapToCube v (as ': ass) = (v $|$ as) ': MapToCube v ass
    MapToCube _ '[]         = '[]

type family ExplodeCube (aspects :: [Type]) :: [[Type]] where
    ExplodeCube (a ': as) = ExplodeAspects a ': ExplodeCube as
    ExplodeCube '[]       = '[]

type family ExplodeAspects (aspect :: Type) :: [Type] where
    ExplodeAspects (Aspect d (v ': vs)) = Aspect d v : ExplodeAspects (Aspect d vs)
    ExplodeAspects (Aspect d '[])       = '[]

-- combine cube

type family CombineCube (aspects :: [[Type]]) :: [[Type]] where
    CombineCube (as ': ass) = MapExplode as (CombineCube ass)
    CombineCube '[]         = '[ '[] ]

type family MapExplode (aspects :: [Type]) (ass :: [[Type]]) :: [[Type]] where
    MapExplode xs (y ': ys) = AppendList (Explode xs y) (MapExplode xs ys)
    MapExplode xs '[]       = '[]

type family Explode (xs :: [Type]) (ys :: [Type]) :: [[Type]] where
    Explode (x ': xs) ys = (x ': ys) ': Explode xs ys
    Explode '[] _ = '[]

-- operator lists

type family AspectList (aspects :: Type) :: [Type] where
    AspectList (x |$ xs) = x ': AspectList xs
    AspectList Nil       = '[]

type family CubeList aspects :: [Type] where
    CubeList (x &&| xs)  = x ': CubeList xs
    CubeList Nil         = '[]

type family Append (xs :: [Type]) (ys :: [Type]) :: [Type] where
    Append '[] ys = ys
    Append (x ': xs) ys = x ': Append xs ys

type family AppendList (xs :: [[Type]]) (ys :: [[Type]]) :: [[Type]] where
    AppendList '[] ys = ys
    AppendList (x ': xs) ys = x ': AppendList xs ys

type family Null ls :: Bool where
    Null '[] = 'True
    Null a   = 'False

type family IfThenElse (cond :: Bool) a b where
    IfThenElse 'True  x y = x
    IfThenElse 'False x y = y

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
             (Proxy :: Proxy (Aspect "foo" "baz" |$ Aspect "egon" "balders" |$ Nil))
             3.3
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
