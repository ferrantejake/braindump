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

# Examples

---

## catalogserver

A key/value database

```erlang
-module(catalogserver).
-export([start/0, server/1]).

start() ->
    Table = [],
    spawn(?MODULE, server, [Table]).

server(Table) -> 
    receive 
        { Pid, { associate, Key, Value }} ->
            % Remove an item from a list using filter
            RemoveFromList = fun(Match, List) -> 
                lists:filter(fun({K, _}) ->
                    case K of
                        Match -> false;
                        _ -> true
                    end
                end, List)
            end,

            % Remove value, add new value
            NewTable = RemoveFromList(Key, Table) ++ [{Key, Value}],             
            Pid ! { self(), ok }, 
            server(NewTable);
        { Pid, { lookup, Key }} ->   
            % Gets item from list
            Items = lists:filter(fun({K, _}) ->
                case K of Key -> true;
                            _ -> false
                end 
            end, Table),

            case length(Items) of
                0 -> 
                    Pid ! { self(), { value_is, undefined }}, 
                    server(Table);
                _ -> 
                    [{_, Value}] = Items,
                    Pid ! { self(), { value_is, Value }}, 
                    server(Table)
            end
    end.end.
```

## sharedvarserver

A server with a state

```erlang
-module(sharedvarserver).
-export([start/1, server/1]).

start(V) -> 
    spawn(?MODULE, server, [V]).

server(S) ->
    receive 
        { Pid, { run, F }} -> 
            NS = F(S),
            Pid ! { self(), { result, NS}},
            server(NewS);
        { Pid, see } ->
            Pid ! S,
            server(S) 
    end.
```



