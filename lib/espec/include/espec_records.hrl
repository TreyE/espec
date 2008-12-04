-record(especTestBinding, {
    lhs_val="",
    rhs_val="",
    lhs_string_val="",
    rhs_string_val=""                       
}).

-record(especMatcherResult, {
    test_result="",
    failure_message="",
    negative_failure_message=""
}).

-record(especExpectationResult, {
    passed="",
    failure_message=""
}).
