-module(json_tests).
-include_lib("eunit/include/eunit.hrl").

json_test_() ->
  [
    ?_assertEqual({module, json}, code:ensure_loaded(json)),
    ?_assert(true)
  ].
