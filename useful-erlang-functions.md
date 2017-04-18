# Overview

A quick synopsis of functions I've personally found helpful using Erlang

| lists:member\(Elem, List\) -&gt; boolean\(\) | true if member is in list |
| :--- | :--- |
| lists:foldr\(Fun, Acc, List\) -&gt; Any,                                                 lists:foldr\(Fun, Acc, List\) -&gt; Any  | fold functions |
|  |  |

## Details

### foldr/foldl

**Source: **[https://www.proctor-it.com/erlang-thursday-lists-foldl-3-and-lists-foldr-3/](https://www.proctor-it.com/erlang-thursday-lists-foldl-3-and-lists-foldr-3/)

```
% ======== FOLDL ==========

lists:foldl(fun(X, Sum) -> Sum + X end, 0, [1, 2, 3, 4, 5]).
% 15
lists:foldl(fun(X, Product) -> Product * X end, 1, [1, 2, 3, 4, 5]).
% 120
lists:foldl(fun(X, Accum) -> io:format("~p ", [X]) end, void, [1, 2, 3, 4, 5]).
% 1 2 3 4 5 ok
lists:foldl(fun(X, Accum) -> io:format("~p ", [X]), Accum end, void, [1, 2, 3, 4, 5]).
% 1 2 3 4 5 void
lists:foldl(fun(X, Accum) -> Accum + X end, 1, []).               
% 1
lists:foldl(fun(X, Result) -> lists:umerge(Result, X) end, [], [[1, 2, 3], [3, 5, 8], [11, 13, 17]]).
% [1,2,3,5,8,11,13,17]

%%======== FOLDR ==========

lists:foldr(fun(X, Accum) -> io:format("~p ", [X]), Accum end, void, [1, 2, 3, 4, 5]).
% 5 4 3 2 1 void
lists:foldr(fun(X, Accum) -> Accum + X end, 1, []).
% 1
```



