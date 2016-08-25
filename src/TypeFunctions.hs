{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module TypeFunctions
  ( CellTypeSingle
  , CellTypeList
  , ExplodeQuery
  , Aspect
  , Nil
  , type (&&|)
  , type ($|$)
  , type (|$)
  ) where

import Data.Type.List (Find, Remove)
import Data.Kind      (Type)

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
data QueryTypeMismatch a b

data Match a
  = NoMatch
  | Matched a
  | MatchMalformedModel

-- type functions

-- | given a database model and a singleton query, return the query result type
--
-- singleton query example
-- > type Query = Aspect "Foo" "bar" |$ Aspect "Cus" "eur"
--
type family CellTypeSingle (model :: Type) (query :: Type) :: Type where
    CellTypeSingle m q = MatchUnwrap (CellTypeMatch m (AspectList q))

-- | given a database model and a list query, return the query result type
type family CellTypeList (model :: Type) (query :: Type) :: Type where
    CellTypeList m q = MatchListUnwrap (MapCellTypeMatch m (ExplodeQuery q)) 'Nothing

-- list query

type family ExplodeQuery (aspects :: Type) :: k where
    ExplodeQuery as = CombineAspects (ExplodeCube (AspectList as))

type family MatchListUnwrap (match :: [Match Type]) (found :: Maybe Type) :: Type where
    MatchListUnwrap ('NoMatch ': ms)   found     = MatchListUnwrap ms found
    MatchListUnwrap ('Matched a ': ms) 'Nothing  = MatchListUnwrap ms ('Just a)
    MatchListUnwrap ('Matched a ': ms) ('Just a) = MatchListUnwrap ms ('Just a)
    MatchListUnwrap ('Matched a ': ms) ('Just b) = QueryTypeMismatch a b
    MatchListUnwrap '[]                ('Just a) = a
    MatchListUnwrap '[]                'Nothing  = QueryNoMatch
    MatchListUnwrap ('MatchMalformedModel ': _) _ = MalformedModel

type family MapCellTypeMatch (model :: Type) (queries :: [[Type]]) :: [Match Type] where
    MapCellTypeMatch m (as ': ass) = CellTypeMatch m as ': MapCellTypeMatch m ass
    MapCellTypeMatch _ '[]         = '[]

-- single query

type family MatchUnwrap (a :: Match Type) :: Type where
    MatchUnwrap ('Matched a)         = a
    MatchUnwrap 'NoMatch             = QueryNoMatch
    MatchUnwrap 'MatchMalformedModel = MalformedModel

type family CellTypeMatch (model :: Type) (aspects :: [Type]) :: Match Type where
    CellTypeMatch cs as = FindOne (MapMatch as (MapExplodeCube (CubeList cs))) 'Nothing

type family MapMatch (query :: [Type]) (cubes :: [Type]) :: [Maybe Type] where
    MapMatch q ((v $|$ as) ': cs) =
      ( IfThenElse (SameLists as q)
                   ('Just v) 'Nothing
      ) ': MapMatch q cs
    MapMatch _ '[]       = '[]

type family FindOne (ls :: [Maybe Type]) (found :: Maybe Type) :: Match Type where
    FindOne '[]              'Nothing  = 'NoMatch
    FindOne '[]              ('Just a) = 'Matched a
    FindOne ('Just a  ': xs) 'Nothing  = FindOne xs ('Just a)
    FindOne ('Just a  ': _)  ('Just b) = 'MatchMalformedModel
    FindOne ('Nothing ': xs) found     = FindOne xs found

-- explode model

type family MapExplodeCube (cubes :: [Type]) :: [Type] where
    MapExplodeCube ((v $|$ as) ': cs) =
      Append (MapToCube v (CombineAspects (ExplodeCube (AspectList as))))
             (MapExplodeCube cs)
    MapExplodeCube '[] = '[]

type family MapToCube (value :: Type) (aspects :: [[Type]]) :: [Type] where
    MapToCube v (as ': ass) = (v $|$ as) ': MapToCube v ass
    MapToCube _ '[]         = '[]

-- explode cube

type family ExplodeCube (aspects :: [Type]) :: [[Type]] where
    ExplodeCube (a ': as) = ExplodeAspects a ': ExplodeCube as
    ExplodeCube '[]       = '[]

type family ExplodeAspects (aspect :: Type) :: [Type] where
    ExplodeAspects (Aspect d (v ': vs)) = Aspect d v : ExplodeAspects (Aspect d vs)
    ExplodeAspects (Aspect d '[])       = '[]

-- combine cube

type family CombineAspects (aspects :: [[Type]]) :: [[Type]] where
    CombineAspects (as ': ass) = MapExplode as (CombineAspects ass)
    CombineAspects '[]         = '[ '[] ]

type family MapExplode (aspects :: [Type]) (ass :: [[Type]]) :: [[Type]] where
    MapExplode xs (y ': ys) = Append (Explode xs y) (MapExplode xs ys)
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

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[] ys = ys
    Append (x ': xs) ys = x ': Append xs ys

type family Null ls :: Bool where
    Null '[] = 'True
    Null a   = 'False

type family IfThenElse (cond :: Bool) a b where
    IfThenElse 'True  x y = x
    IfThenElse 'False x y = y

type family SameLists (xs :: [k]) (ys :: [k]) :: Bool where
    SameLists (x ': xs) ys = IfThenElse (Find x ys) (SameLists xs (Remove x ys)) 'False
    SameLists '[]      '[] = 'True
    SameLists '[]      _   = 'False
