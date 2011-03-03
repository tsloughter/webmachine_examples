%%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan.sloughter@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2011 Tristan Sloughter
%%%----------------------------------------------------------------
-module(we_db).

-behaviour(gen_server).

%% API
-export([start_link/3,
         all/1,
         find/2,
         create/2,
         update/3,
         terminate/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export_type([]).

-define(SERVER, ?MODULE).

-record(state, {db}).

%%%===================================================================
%%% Public Types
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================

start_link(Server, Port, DB) ->
    gen_server:start_link(?MODULE, [Server, Port, DB], []).

all(PID) ->
    gen_server:call(PID, all).

find(PID, ID) ->
    gen_server:call(PID, {find, ID}).

create(PID, Doc) ->
    gen_server:call(PID, {create, Doc}).

update(PID, ID, JsonDoc) ->
    gen_server:call(PID, {update, ID, JsonDoc}).

terminate(PID) ->
    gen_server:call(PID, terminate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([Server, Port, DB]) ->
    CouchServer = couchbeam:server_connection(Server, Port, "", []),
    {ok, CouchDB} = couchbeam:open_db(CouchServer, DB),
    {ok, #state{db=CouchDB}}.

%% @private
handle_call(all, _From, #state{db=DB}=State) ->
    Docs = get_docs(DB, [{descending, true}]),
    {reply, mochijson2:encode(Docs), State};
handle_call({find, ID}, _From, #state{db=DB}=State) ->
    [Doc] = get_docs(DB, [{key, list_to_binary(ID)}]),
    {reply, mochijson2:encode(Doc), State};
handle_call({create, Doc}, _From, #state{db=DB}=State) ->
    {ok, Doc1} = couchbeam:save_doc(DB, Doc),
    {NewDoc} = couchbeam_doc:set_value(<<"id">>, couchbeam_doc:get_id(Doc1), Doc1),
    {reply, mochijson2:encode({struct, NewDoc}), State};
handle_call({update, ID, NewDoc}, _From, #state{db=DB}=State) ->
    IDBinary = list_to_binary(ID),
    {ok, Doc} = couchbeam:open_doc(DB, IDBinary),
    NewDoc2 = couchbeam_doc:set_value(<<"_id">>, IDBinary, {NewDoc}),
    NewDoc3 = couchbeam_doc:set_value(<<"_rev">>, couchbeam_doc:get_rev(Doc), NewDoc2),
    {ok, {Doc1}} = couchbeam:save_doc(DB, NewDoc3),
    {reply, mochijson2:encode({struct, Doc1}), State};
handle_call(terminate, _From, State) ->
    {stop, normal, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_docs(DB, Options) ->
    {ok, AllDocs} = couchbeam:view(DB, {"all", "find"}, Options),
    {ok, Results} = couchbeam_view:fetch(AllDocs),

    {[{<<"total_rows">>, _Total},
      {<<"offset">>, _Offset},
      {<<"rows">>, Rows}]} = Results,

    lists:map(fun({Row}) ->
                      {<<"value">>, {Value}} = lists:keyfind(<<"value">>, 1, Row),
                      {struct, Value}
              end, Rows).

