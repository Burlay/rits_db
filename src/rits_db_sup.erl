-module(rits_db_sup).
-behaviour(supervisor).

-export([start_link/0,
    init/1
  ]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Pools = [{pool1,  [
        {size, 10},
        {max_overflow, 20}
      ], [
        {hostname, "127.0.0.1"},
        {database, "rits_db"},
        {username, "brasing"},
        {password, "postgres"}
      ]}
    ],
  PoolSpecs = lists:map(
    fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [
          {name, {local, Name}},
          {worker_module, rits_db_worker}
        ] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end,
    Pools),
  {ok, {{one_for_one, 5, 10}, PoolSpecs}}.
