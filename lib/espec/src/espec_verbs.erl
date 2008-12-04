-module(espec_verbs).

%%
%% Include files
%%
-include("../include/espec_common.hrl").
-include("../include/espec_records.hrl").

%%
%% Exported Functions
%%
-export([should_not/1, should/1]).

%%
%% API Functions
%%

should_not(TB) ->
    C = TB#especTestBinding.rhs_val, 
    R = C(TB),
    TR = (not passed(R)),
    case (TR) of
              true -> #especExpectationResult{ passed = true };
              _ -> #especExpectationResult{ 
                          passed = false,
                          failure_message = R#especMatcherResult.negative_failure_message
                       }
     end.

should(TB) ->
    C = TB#especTestBinding.rhs_val, 
    R = C(TB),
    TR = passed(R),      
    case (TR) of
              true -> #especExpectationResult{ passed = true };
              _ -> #especExpectationResult{ 
                          passed = false,
                          failure_message = R#especMatcherResult.failure_message
                       }
     end.

passed(Val) ->
  Val#especMatcherResult.test_result.
