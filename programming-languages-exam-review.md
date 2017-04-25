# Tables of Contents

* Bounded and Unbounded Variables

## Free and Bound Variables

```Haskell
concat(map(fun(Y) -> F(X,Y) end, (fun(Q,R) -> Q end)(LS,empty)))
```

### Bound

```
{ Y, Q }
```

### Free

```
{ X, LS, F, concat, map }
```

Notice that `concat` and `map` are free in this context because they are declared in some other module and not constrained to the current context.

### Neither

```
{ R, empty }
```

R is never used. empty is an atom. From this information it follows that these structures are either neither bounded nor unbounded variables, as they were never variables to begin with.

