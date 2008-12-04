-module(espec_test).

-export([be_implemented_in/1]).

%%
%% Include files
%%
-include("espec.hrl").
-include("espec_common.hrl").

%%
%% Test Matchers
%%

be_implemented_in(Mod_name) ->
    fun(Test_binding) ->
      Lhs_val = espec_test_binding:lhs(Test_binding),
      Lhs_string_val = espec_test_binding:lhs_string(Test_binding),
    B = lists:append(
         [Mod_name],
         lists:map(
           fun(X) -> case regexp:match(X, "[A-Za-z]") of nomatch -> list_to_integer(X); _ -> list_to_atom(X) end end,
           element(2, regexp:split(Lhs_val, "/"))
         )                         
    ),
    #especMatcherResult{
      test_result = apply(erlang, function_exported, B),
      failure_message = ?L_STR([
             "Expected ", Lhs_string_val,
             " to be implemented in module ", Mod_name,
             " but it was not"
      ]),
      negative_failure_message = ?L_STR([
             "Expected ", Lhs_string_val,
             " to not be implemented in module ", Mod_name,
             " but it was"
      ])
    }
    end.
    
%%
%% Test Functions
%%

custom_matcher_in_test_module_test() ->                               
    ?specify("start/0", should, be_implemented_in(espec)).
