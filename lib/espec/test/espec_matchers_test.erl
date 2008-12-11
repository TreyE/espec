-module(espec_matchers_test).

%%
%% Include files
%%
-include("espec.hrl").

%%
%% Test Functions
%%

raise_error_type_matcher_test() ->
    ErrorFun = fun() ->
                 erlang:error(blah)
               end,
    ?specify(ErrorFun, should, raise_error(blah)).
