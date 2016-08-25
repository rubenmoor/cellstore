# about

A database interface for document stores (like MongoDB) with type-safe queries.
The idea is to have a database model as a type and have the compiler check
the validity of the database queries.

Also, the database model is inspired by the idea of a cellstore.
The database model is conceived as list of *datapoints*.
At each datapoint an object of a certain type is stored (as document).
Datapoints are specified as lists of *aspects* and each aspect is a pair of
a *dimension* and a *dimensional value*.

Queries for objects in the database are constructed by specifying a datapoint.

This is work-in-progress.
See `SimpleModel` and `test` in `src/Lib.hs` to get an idea.

# build

There is a stack.yaml, so

```shell
stack build
stack exec cellstore-exe
```
