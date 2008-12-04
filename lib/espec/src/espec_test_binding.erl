-module(espec_test_binding).

-export([new/4, lhs/1, lhs_string/1, rhs/1, rhs_string/1]).

-include("espec_records.hrl").

new(LHS, RHS, LHSStr, RHSStr) ->
    #especTestBinding{
               lhs_val = LHS,
               rhs_val = RHS,
               lhs_string_val = LHSStr,
               rhs_string_val = RHSStr
    }.

lhs(ETB) ->
    ETB#especTestBinding.lhs_val.

rhs(ETB) ->
    ETB#especTestBinding.rhs_val.

lhs_string(ETB) ->
    ETB#especTestBinding.lhs_string_val.

rhs_string(ETB) ->
    ETB#especTestBinding.rhs_string_val.
