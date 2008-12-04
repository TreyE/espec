%% Author: trey
%% Created: Apr 15, 2008
%% Description: TODO: Add description to espec_formatter
-module(espec_formatter).

%%
%% Exported Functions
%%
-export([format_term/1]).

%%
%% Include files
%%
-include("../include/espec_common.hrl").

%%
%% API Functions
%%

format_term(Val) -> term_value(Val).

%%
%% Local Functions
%%


%% A quick and dirty term formatting function.
term_value(Val) when is_binary(Val) ->
    lists:concat(["binary ", lists:flatten(io_lib:write(Val))]); 
term_value(Val) when is_pid(Val) ->
    lists:concat(["pid ", pid_to_list(Val)]);
term_value(Val) when is_tuple(Val) ->
    ?L_STR([
    "{ ",
       remove_trailing_string_comma(?L_STR(
                    lists:map(
                            fun(X) -> 
                                ?L_STR([term_value(X), ", "])
                            end,
                            tuple_to_list(Val)
                           )
      )),
    " }"
    ]);
term_value(Val) ->
    Val.

remove_trailing_string_comma(Str) ->
    string:strip(string:strip(Str, right, $ ), right, $,).
