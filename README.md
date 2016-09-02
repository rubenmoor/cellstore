# about

A database interface for document stores (like MongoDB) with type-safe queries.
The idea is to have a database model as a type and have the compiler check
the validity of the database queries.

In addition, the database model is inspired by the idea of a cellstore.
The database model is conceived as list of *datapoints*.

``` haskell
type MyModel = Double           $|$ ...
           &&| Maybe Integer    $|$ ...
           &&| Map Integer Text $|$ ...
```

At each datapoint an object of a certain type is stored (as document).
The object can be any Haskell type, given it has - in the case of
MongoDB as backend - a suitable `BSON` instance.

Datapoints are specified as lists of *aspects* and each aspect is a pair of
a *dimension* and a list of *dimensional values*.

```haskell
type MyModel = Double        $|$ Aspect "Category" '["Monetary"]
                              |$ Aspect "Currency" '["EUR", "USD", "ZWL"]
           &&| Maybe Integer $|$ Aspect "Category" '["Rank"]
                              |$ Aspect "Currency" '["EUR", "USD", "ZWL"]
           &&| ...
```

Queries for objects in the database are types that are constructed by
specifying a datapoint.

``` haskell
type AQuery = Aspect |$ "Category" "Monetary"
                     |$ "Currency" "EUR"
```

The compiler can infer the type of the query result given a model
(`Integer`, in the example).

More complex queries use type-level lists for the dimensional values, resulting
in lists as result type ...


``` haskell
type AListQuery = Aspect |$ "Category" '["Monetary"]
                         |$ "Currency" '["EUR", "USD", "ZWL"]
```

Or tuples (`(Double, Maybe Integer)` in the example,
no idea how implement that, yet).

``` haskell
type ATupleQuery = Aspect |$ "Category" '["Monetary", "Rank"]
                          |$ "Currency" '["EUR"]
```

This is work-in-progress.
See `SimpleModel` and `test` in `src/Lib.hs` to get an idea.

# build

There is a stack.yaml, so

```shell
stack build
stack exec cellstore-exe
```
