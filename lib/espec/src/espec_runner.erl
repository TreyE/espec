%%%-------------------------------------------------------------------
%%% File    : espec_runner.erl
%%% Description : Runner methods for starting espec against a project
%%%
%%% Created :  4 Dec 2008 by Trey <trey@treysoldcomp>
%%%-------------------------------------------------------------------
-module(espec_runner).

%% API
-export([run/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function:
%% Description:
%%--------------------------------------------------------------------
run() ->
    {ok, PPath} = application:get_env(espec, build_dir),
    BRef = get_build_ref(),
    sinan:do_task(espec, BRef, sinan_server_args(PPath)).

%%====================================================================
%% Internal functions
%%====================================================================

sinan_server_args(ProjPath) ->
  { obj,
    [{ "build",
      { obj,
        [{"start_dir", ProjPath}]
      }
    }]
  }.

get_build_ref() -> sinan:gen_build_ref().
