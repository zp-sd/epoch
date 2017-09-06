-module(aecore_app_tests).

-include_lib("eunit/include/eunit.hrl").

rocksdb_dep_smoke_test_() ->
    {setup,
     fun () -> mktempd() end,
     fun (TmpDir) -> file:delete(TmpDir) end,
     fun(TmpDir) ->
             file:open
     end
    }.

mktempd() ->
    mktempd(os:type()).

mktempd({unix, _}) ->
    lib:nonl(?cmd("mktemp -d")).
