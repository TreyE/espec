%% This is the application resource file (.app file) for the espec,
%% application.
{application, espec, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [
              espec_app,
              espec_sup,
              espec,
              espec_formatter,
              espec_matchers,
              espec_runner,
              espec_verbs,
              sin_es_build,
              sin_espec
              ]},
   {registered,[espec_sup]},
   {applications, [kernel, stdlib]},
   {versioned_dependencies, [
     {etask, "0.5.0", gte},
     {eunit, "2.0", gte},
     {sinan, "0.10.0.10", gte}
   ]},
   {mod, {espec_app,[]}},
   {start_phases, []}]}.

