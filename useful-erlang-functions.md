# Preface

Here are where I am pulling many of these definitions: [http://erlang.org/doc/man/lists.html\#foldr-3](http://erlang.org/doc/man/lists.html#foldr-3)  
LearnYouSomeErlang is an extremely useful document: [http://learnyousomeerlang.com](http://learnyousomeerlang.com)

## Handy Functions

A quick synopsis of functions I've personally found helpful using Erlang

| Function | Parameters | Description |
| :--- | :--- | :--- |
| lists:member/2 | Elem, List-&gt; boolean | true if member |
| lists:foldr/3 | fun, Acc, List -&gt; List | fold functions |
| lists:sum/1 | List: number\[\] | sum a list |
| lists:droplast/1 | List: any\[\] | drop last element |
|  |  |  |

---

## Syntax

### Case

```
case <expr> of
    <pat 1> -> <body1> ;
    <pat 2> -> <body2> ;
    ...
    <pat n> -> <bodyn> 
end
```

## If

```
if 
    some_condition -> some_code;
    some_other_condition -> some_other_code;
    true -> ok
end.
```

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



