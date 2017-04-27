# Concepts

---

## Free and Bound Variables

---

### Question

```Haskell
concat(map(fun(Y) -> F(X,Y) end, (fun(Q,R) -> Q end)(LS,empty)))
```

### Answer

```
bound:    { Y, Q }
free:     { X, LS, F, concat, map }
neither:  { R, empty }
```

Notice that `concat` and `map` are free in this context because they are declared in some other module and not constrained to the current context.

R is never used. empty is an atom. From this information it follows that these structures are either neither bounded nor unbounded variables, as they were never variables to begin with.

### Question

```
G(foo(G, G(length([zero, true, fun(X,Y) -> X(3) end]))))
```

### Answer

```
free:     { G, foo, length }
bound:    { X } 
neither:  { Y }
```

### Question

```erlang
A(fun(A,B) -> A(foo(bar({what, fun(X,Y) -> plus(Y,3) end}))) end, I, J
```

### Answer

```
free:     { I, J, A, foo, bar, plus }
bound:    { A, Y } 
neither:  { X }
```

## Q & A

---

#### General

1. How are variable names in Erlang scoped \(statically or dynamically\)?
   1. **Dynamically**
2. Suppose a programming language has a way to create function values at runtime, like Erlang does. Explain how you can you tell if that language has static or dynamic scoping for variable names?
   1. **something**
3. If a language makes closures for function values that are created at runtime, what kind of identifiers that occur within the text of the function’s code does it need to remember values for: free identifiers or bound identifiers?
   1. **bound variables**
4. Is static typing or dynamic typing more useful? Explain why \(or why not\).
   1. **Depends on your application. I would argue that dynamic typing is more useful because you have more freedom as to what a variable can be. For example, dynamic typing allows lists to contain data of different types. This is something handy which static typing does not allow**
5. Is static scoping more useful than dynamic scoping for variables? Explain why \(or why not\).
   1. **Many programming languages - and for good reason - use static scoping. This is because it is easier to reason about in the code. In terms of being able to easily understand a program, static typing would be more useful to the programmer.**
6. How are exception handlers in Java \(or C\# or C++\) scoped \(statically or dynamically\).
   1. **Statically. Java requests an exception type in the syntax.**

#### Questions about type checking

1. By default, does Erlang do static or dynamic type checking?
   1. **Dynamic**
2. What kind of type checking does Haskell do: static or dynamic?
   1. **Dynamic**
3. Give a brief example of an expression in Erlang that generates a type error.
   1. **Attempting to read data using an invalid pattern matching pattern** 
4. Which kind of type checking allows the programmer more flexibility: static or dynamic type checking? 
   1. **Dynamic**
5. Give an example, in Erlang, of an expression or program that will run without a type error that would not type check if it were translated into Haskell.
   1. `P = fun(Obj, Acc) -> Acc end, lists:foldr(P, void, [{key, value}]).`
      1. This would run without issue because the object `{key, value}` can be generalized into a single entity, `Obj` . If this ran in Haskell, it would not get type checked because `Obj` is never used.
6. In Erlang, does the representation of every value need to be encoded in such a way that the runtime system can tell what its type is during program execution?
   1. **No, items can be talked about abstractly. See above.**
7. In Haskell, does the representation of every value need to be encoded in such a way that the runtime system can tell what its type is during program execution
   1. **No, items can be talked about abstractly. See above.**
8. Are parameters in Haskell and Erlang passed by reference? How do we know this?
   1. No, they are passed by value. 
9. How is functional \(declarative\) programming different from imperative programming?  
   1. **Imperative programming **telling the "machine" _how_ to do something, and as a result _what_ you want to happen will happen  
   2. **Declarative programming **telling the "machine" _what_ you would like to happen, and let the computer figure out \_how \_to do it

   For example, take the following SQL query

```sql
SELECT * from dogs
INNER JOIN owners
WHERE dogs.owner_id = owners.id
```

SQL is a declarative language. Here we have abstracted away from how to do something and we focus more on what we want done. We let the machine worry about the best way \(_how_\) to go about getting that task done.

#### Modules, Data Abstraction, Information Hiding

1. Briefly describe how one could implement an abstract data type, such as a stack of items, in Erlang in such a way that the internal implementation details of the stack are completely hidden? \(You don’t have to provide code, but you can show what it would look like if you wish.\)
   1. Using a high order function, you could manipulate the contents of a requested data structure using a passed function

```erlang
fun(List, F) -> lists:map(F, List).
```

1. How is representing an abstract data type, such as a stack of items different in Haskell than in Erlang?
   1. &lt;something&gt;
2. Is data abstraction \(the hiding of implementation details\) enforced in Java \(or C\#\) in a way that is more like Haskell or more like Erlang? Choose either Haskell or Erlang, and give a brief explanation of why.
   1. &lt;something&gt;

## Types

---

### Question

In Erlang, will there necessarily be a type error if one puts different types of elements in a single list, like the following?

```erlang
[3, true, foo, '$c', [1,2], [ok,sure], 5.2, self()]
```

Answer “yes” or “no” and give a brief justification:

#### Answer

No, dynamic typing allows Erlang to have lists composed of different types. Type is known at runtime for each element of the list. The only case where one would receive a type error is trying to access the data using inappropriate pattern matching.

# Examples

---

## catalogserver

A key:value database

```erlang
% Jake Ferrante
% key:value pair model

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
% Jake Ferrante
% Shared state server model

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

## election

Elect jake for president

```erlang
% Jake Ferrante
% Election system

-module(election). 
-export([start/0, vote/2, results/1]).

% Start an election server with 0 votes cast.
start() -> 
    Candidates = [],
    Ballots = [],
    spawn(fun() -> server(Candidates, Ballots) end).

% Patterns:
% {PID, vote, Candidate}    -> add candidate and vote
% {PID, results}            -> return ballot tuples
server(Candidates, Ballots) ->
    receive
         % Respond with list of tuples
        { PID, results } -> 
            PID ! { self(), ballots, Ballots },
            server(Candidates, Ballots);
        { PID, vote, Candidate } ->
            case lists:member(Candidate, Candidates) of 
                % If candidate exits, increment votes
                true -> 
                    P = fun({C, V}, Acc) -> 
                            if C == Candidate -> Acc ++ [{C, V+1}]
                             ; true -> Acc ++ [{C, V}] end 
                        end,
                    NewBallots = lists:foldl(P, [], Ballots),
                    PID ! { self(), ok },
                    server(Candidates, NewBallots);
                % If candidate DNE, add to ballot with 1 vote
                false -> 
                    NewCandidates = Candidates ++ [Candidate],
                    NewBallots = Ballots ++ [{Candidate, 1}],
                    PID ! { self(), ok },
                    server(NewCandidates, NewBallots)
            end
    end.
```

## recursion

regular and tail recursion

```erlang
-module(recursion).
-export([mylength/1, len/1, len/2]).

% Regular recursion
mylength([])      -> 0; 
mylength([_ | T]) -> 1 + mylength(T).

% Tail recursive function
len(Ls)         -> len(Ls, 0).
len([], N)      -> N;
len([_|T],N)    -> len(T,N+1).
```



