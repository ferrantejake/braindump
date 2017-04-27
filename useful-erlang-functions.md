# Preface

---

Here are where I am pulling many of these definitions: [http://erlang.org/doc/man/lists.html\#foldr-3](http://erlang.org/doc/man/lists.html#foldr-3)  
LearnYouSomeErlang is an extremely useful document: [http://learnyousomeerlang.com](http://learnyousomeerlang.com)

## Handy Functions

---

A quick synopsis of functions I've personally found helpful using Erlang

| Function | Parameters | Description |
| :--- | :--- | :--- |
| lists:member/2 | Elem, List-&gt; boolean | true if member |
| lists:foldr/3 | fun, Acc, List -&gt; List | fold functions |
| lists:sum/1 | List: number\[\] | sum a list |
|  | List: any\[\] | drop last element |
|  |  |  |
|  |  |  |

# Syntax

---

## Case

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

## 



