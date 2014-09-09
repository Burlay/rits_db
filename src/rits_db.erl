-module(rits_db).
-behaviour(application).

%% rits_db: rits_db library's entry point.

-export([
    start/2,
    stop/1,
    query/2,
    query/3
  ]).


%% API

start(_StartType, _StartArgs) ->
  Name = database_fuse,
  Strategy = {standard, 5, 5000},
  Refresh = {reset, 60000},
  Opts = {Strategy, Refresh},
  fuse:install(Name, Opts),
  rits_db_sup:start_link().

stop(_State) ->
  ok.

query(PoolName, Sql) ->
  case fuse:ask(database_fuse, sync) of
    ok ->
      case squery(PoolName, Sql) of
        {error, _Error} ->
          io:format("ERROR: ~p~n", [_Error]),
          ok = fuse:melt(database_fuse);
        Result ->
          Result
      end;
    blown ->
      {error, fuse_blown}
  end.

query(PoolName, Stmt, Params) ->
  case fuse:ask(database_fuse, sync) of
    ok ->
      case equery(PoolName, Stmt, Params) of
        {error, _Error} ->
          io:format("Error: ~p~n", [_Error]),
          ok = fuse:melt(database_fuse);
        Result ->
          Result
      end;
    blown ->
      {error, fuse_blown}
  end.

%% Internals

squery(PoolName, Sql) ->
  poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {squery, Sql})
    end).

equery(PoolName, Stmt, Params) ->
  poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {equery, Stmt, Params})
    end).

%% End of Module.
