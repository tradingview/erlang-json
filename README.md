erlang-eep0018
==============

Json library for Erlang.


Features
--------

* eep-0018 style decoded term.
* can decode map key in atom (default is binary).
* implemented in NIF.
* c-style comments acceptable (/* ... */) (default is denied).
* bigint supported.

Compiling
---------

    $ make

Assuming rebar works for you that should build everything. Yay.

Testing
-------

    $ make check

Hopefully the tests pass.

Usage
-----

Put this app in your Erlang path.

    $ erl -pa ebin/
    Erlang R15B (erts-5.9) [source] [64-bit] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V5.9  (abort with ^G)
    1> json:decode(<<"{\"foo\": true}">>).
    {ok,{[{<<"foo">>,true}]}}
    2> json:encode([true, 1.2, null]).
    {ok,<<"[true,1.2,null]">>}

Yeah. It's that easy.

Repository
----------

* https://github.com/hio/erlang-eep0018
