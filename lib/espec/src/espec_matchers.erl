-module(espec_matchers).
-include("../include/espec_common.hrl").
-include("../include/espec_records.hrl").

-define(FORMAT(Val), espec_formatter:format_term(Val)).
%%
%% Exported Functions
%%
-export([eql/1, include/1, be_true/0, be_undefined/0, be_a_globally_registered_name/0, find_matcher/2, raise_error/1]).

%%
%% Macros
%%

-define(LHS(ETB), espec_test_binding:lhs(ETB)).
-define(LHS_STR(ETB), espec_test_binding:lhs_string(ETB)).


%%
%% API Functions
%%

%%
%% TODO: Add description of eql/function_arity
%%

find_matcher(Fun_Name, _Mod_name) when is_function(Fun_Name) ->
    Fun_Name;
find_matcher(Fun_Name, Mod_name) ->
    case erlang:function_exported(Mod_name, Fun_Name, 0) of
      false -> ({ espec_matchers, Fun_Name })();
      _ -> ({ Mod_name , Fun_Name })()
    end.

eql(A) ->
    fun(Test_binding) ->
      Lhs_val = ?LHS(Test_binding),
      Lhs_string_val = ?LHS_STR(Test_binding),
      #especMatcherResult{
       test_result = ( A == Lhs_val ),
       failure_message = ?L_STR(
         [
           "Expected ", Lhs_string_val,
            " to be equal to ", ?FORMAT(A),
            " but was defined as ", ?FORMAT(Lhs_val), "."
         ]),
       negative_failure_message = ?L_STR(
         [
           "Expected ", Lhs_string_val,
            " to not be equal to ", ?FORMAT(A),
            " but was." 
         ])
      }
    end.

include(A) ->
    fun(Test_binding) ->
      Lhs_val = ?LHS(Test_binding),
      Lhs_string_val = ?LHS_STR(Test_binding),
      #especMatcherResult{
       test_result = val_is_list_member(A, Lhs_val ),
       failure_message = ?L_STR(
         [
           "Expected ", Lhs_string_val,
            " to include ", ?FORMAT(A),
            " but did."
         ]),
       negative_failure_message = ?L_STR(
         [
           "Expected ", Lhs_string_val,
            " not to include ", ?FORMAT(A),
            " but did."
         ])
      }
    end.

be_undefined() -> 
   fun(Test_binding) ->
    Lhs_val = ?LHS(Test_binding),
    Lhs_string_val = ?LHS_STR(Test_binding),
      #especMatcherResult{
       test_result = (undefined == Lhs_val),
       failure_message = ?L_STR(
         [
           "Expected ", Lhs_string_val,
            " to be undefined, but was defined as",
            ?FORMAT(Lhs_val)
         ]),
       negative_failure_message = ?L_STR(
         [
           "Expected ", Lhs_string_val,
            " to be defined, but was not"
         ])
      }
    end.

be_a_globally_registered_name() -> 
   fun(Test_binding) ->
    Lhs_val = ?LHS(Test_binding),
    Lhs_string_val = ?LHS_STR(Test_binding),
      #especMatcherResult{
       test_result = (true == lists:member(Lhs_val, global:registered_names())),
       failure_message = ?L_STR(
         [
           "Expected ", Lhs_string_val,
            " to be a global name, but it was not found in the global name registry "
         ]),
       negative_failure_message = ?L_STR(
         [
           "Expected ", Lhs_string_val,
            " not to be a global name, but it was found in the global name registry "
         ])
      }
    end.

be_true() -> 
    fun(Test_binding) ->
      Lhs_val = ?LHS(Test_binding),
      Lhs_string_val = ?LHS_STR(Test_binding),
      matcher_result(
       (true == Lhs_val),
       ?L_STR(
         [
           "Expected ", Lhs_string_val,
           " to be true, but was defined as ", ?FORMAT(Lhs_val)
         ]),
       ?L_STR(
         [
           "Expected ", Lhs_string_val,
           " to be not be true, but was defined as ", ?FORMAT(Lhs_val)
         ])
      )
    end.


raise_error(EType) -> 
   fun(Test_binding) ->
    Lhs_val = ?LHS(Test_binding),
    Lhs_string_val = ?LHS_STR(Test_binding),
    Successful = case (catch Lhs_val()) of
      {'EXIT', {EType, _}} -> true;
      _ -> false
    end,
      #especMatcherResult{
       test_result = Successful,
       failure_message = ?L_STR(
         [
           "Expected ", Lhs_string_val,
            " to raise an error of type ", ?FORMAT(EType),
            " but didn't"
         ]),
       negative_failure_message = ?L_STR(
         [
           "Expected ", Lhs_string_val,
            " not to raise an error of type ", ?FORMAT(EType),
            " but did"
         ])
      }
    end.

%%
%% Local Functions
%%
val_is_list_member(Val, Lst) when is_tuple(Lst) -> lists:member(Val, tuple_to_list(Lst));
val_is_list_member(Val, Lst) -> lists:member(Val, Lst).

matcher_result(TestResult, FailureMsg, NegativeFailureMsg) ->
    #especMatcherResult{
       test_result = TestResult,
       failure_message = FailureMsg,
       negative_failure_message = NegativeFailureMsg
    }.
