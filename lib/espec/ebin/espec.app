%% This is the application resource file (.app file) for the espec,
%% application.
{application, espec,
  [{description, "ESpec Application"},
   {vsn, "0.1.0.1"},
   {modules, [
              espec,
              espec_formatter,
              espec_matchers,
              espec_runner,
              espec_test_binding,
              espec_verbs,
              sin_espec_build,
              sin_espec
              ]},
   {registered,[]},
   {applications, [kernel, stdlib]},
   {versioned_dependencies, [
     {etask, "0.5.0", gte},
     {sinan, "0.10.0.14", gte}
   ]},
   {start_phases, []}]}.

