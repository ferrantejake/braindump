# Meaningful output with Erlang

**Article:** [http://noss.github.io/2009/03/29/erlang-io-format-to-string.html](http://noss.github.io/2009/03/29/erlang-io-format-to-string.html)

It is quite often the case that inline debugging is necessary;  Erlang- at least what I know of the tools for it does not have a debugger.

# Article {#article}

---

# Erlang io:format to string

* [Front page](http://noss.github.io/)
* [Github](http://github.com/noss)

2009-03-29

This is quite a[FAQ](http://erlang.org/faq/faq.html)

> How do you get the result from[io:format](http://www.erlang.org/doc/man/io.html#format-3)into a string.

So here goes.

There is an [io\_lib:format](http://www.erlang.org/doc/man/io_lib.html#format-2) which returns the formatted result instead of printing it. Notice io\_lib as module name instead of io. The result is not necessarily a flat list. No. The result might look like the following.

```
1
>
 io_lib:format("foo ~p bar~n", [42]).
[102,111,111,32,"42",32,98,97,114,"\n"]
2
>
 erlang:iolist_to_binary(v(1)).

<
<
"foo 42 bar\n"
>
>

3
>
 lists:flatten(v(1)).
"foo 42 bar\n"

```

But as you see, there are ways to flatten the result. It is not done per automation since all data sent to ports are flattened there anyway. Ports include files, other processes, sockets, etc. The concept is called “iolist”. If you want the byte-count of an iolist, then use [erlang:iolist\_size](http://www.erlang.org/doc/man/erlang.html#iolist_size-1).

Finally I would like to point out a little useful function called [lists:concat](http://www.erlang.org/doc/man/lists.html#concat-1) \(which is nothing like [lists:append](http://www.erlang.org/doc/man/lists.html#append-1)\).

```
4
>
 lists:concat([foo," ", 42, " ", bar, "\n"]).
"foo 42 bar\n"

```

Send comments to chsu79@gmail.com

