-ifndef(ESPEC_HRL).
-define(ESPEC_HRL, true).

-define(TEST, true).
-include_lib("eunit/include/eunit.hrl").
-include("espec_records.hrl").
-import(espec_matchers, [include/1, eql/1, be_true/0, be_undefined/0]).

-define(_specify(A, B, C, Test_Context),
        ((fun () ->
            Vals = espec_test_binding:new(
               A,
               espec_matchers:find_matcher(
                 C,
                 dict:fetch(module, Test_Context)
                ),
               ??A,
               ??C                                              
            ),
            M = dict:fetch(module, Test_Context),
            L = dict:fetch(line, Test_Context),
            R = espec_verbs:B(Vals),
            case (R#especExpectationResult.passed) of
                true -> ok;
                false -> .erlang:error({specification_failed,
                                      [
                	               {module, M},
                                       {line, L},
                                       { failure_message, R#especExpectationResult.failure_message }
                                       ]})
            end
          end)())).


-define(MODULE_UNDER_TEST, list_to_atom(
            lists:nth(
              1,
              element(
                2,
                regexp:split(atom_to_list(?MODULE), "_test"))
              )
          )
).

-define(specify(A, B, C), ?_specify(
   A, B, C,
   dict:from_list([
     {line, ?LINE},
     {module, ?MODULE},
     {module_under_test, ?MODULE_UNDER_TEST}
   ]))).

-endif.
