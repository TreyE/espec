-module(espec).

-export([start/0]).

start() ->
    sinan:start(),
    sin_espec_build:start(),
    sin_espec:start(),
    espec_runner:run().
