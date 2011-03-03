%%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan.sloughter@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2011 Tristan Sloughter
%%%----------------------------------------------------------------
-module(we_db_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/0,
         terminate_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | any().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?SERVER, []).

terminate_child(PID) ->
    ece_db:terminate(PID).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
-spec init(list()) -> {ok, {SupFlags::any(), [ChildSpec::any()]}} |
                       ignore | {error, Reason::any()}.
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    {ok, Server} = application:get_env(server),
    {ok, Port} = application:get_env(port),
    {ok, DB} = application:get_env(database),

    AChild = {we_db, {we_db, start_link, [Server, Port, DB]},
              Restart, Shutdown, Type, [we_db]},

    {ok, {SupFlags, [AChild]}}.

