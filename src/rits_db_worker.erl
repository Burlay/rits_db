-module(rits_db_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

-record(state, {conn}).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  process_flag(trap_exit, true),
  Hostname = proplists:get_value(hostname, Args),
  Database = proplists:get_value(database, Args),
  Username = proplists:get_value(username, Args),
  Password = proplists:get_value(password, Args),
  Conn = pgsql_connection:open(Hostname, Database, Username, Password),
  {ok, #state{conn=Conn}}.

handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->
  {reply, pgsql_connection:simple_query(Sql, [], 500, Conn), State};
handle_call({equery, Stmt, Params}, _From, #state{conn=Conn}=State) ->
  {reply, pgsql_connection:extended_query(Stmt, Params, [], 500, Conn), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
  ok = pgsql_connection:close(Conn),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
