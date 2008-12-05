%% This is the application resource file (.app file) for the espec,
%% application.
{application, espec, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [
              espec,
              espec_formatter,
              espec_matchers,
              espec_runner,
              espec_test_binding,
              espec_verbs,
              sin_es_build,
              sin_espec
              ]},
   {registered,[]},
   {applications, [kernel, stdlib]},
   {versioned_dependencies, [
     {etask, "0.5.0", gte},
     {eunit, "2.0", gte},
     {sinan, "0.10.0.10", gte}
   ]},
   {start_phases, []}]}.

