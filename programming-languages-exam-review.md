# Tables of Contents

* Bounded and Unbounded Variables



## Bounded and Unbounded Variables

```Haskell
concat(map(fun(Y) -> F(X,Y) end, (fun(Q,R) -> Q end)(LS,empty)))
```

### Bounded

```
{ Y, Q concat, map }
```

Note that `concat` and `map`  are bounded because the are declared functions. The notion that a variable is bound is because it is referencing some prior instantiation. In this case, `concat` and `map` have been declared in some other module.

### Unbounded

```
{ X, LS }
```

Neither LS nor X is not bounded in this context.

### Neither

```
{ R, empty }
```

R is never used. empty is an atom. From this information it follows that these structures are either neither bounded nor unbounded variables, as they were never variables to begin with.

