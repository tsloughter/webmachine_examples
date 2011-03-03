-module(we_resource_user).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         finish_request/2,
         from_json/2,
         to_json/2,
         to_html/2,
         process_post/2]).

-include("context.hrl").
-include("jsonerl.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("we_model/include/we_user.hrl").

init([]) ->
    {ok, App}= application:get_application(),
    PrivDir = code:priv_dir(App),
    {ok, PID} = we_db_sup:start_child(),
    {ok, #context{docroot=PrivDir, db=PID}}.

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}, {"text/html", to_html}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    we_db_sup:terminate_child(Ctx#context.db),
    {true, ReqData, Ctx}.

process_post(ReqData, Ctx) ->
    [{JsonDoc, _}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    {struct, Doc} = mochijson2:decode(JsonDoc),
    NewDoc = we_db:create(Ctx#context.db, {Doc}),
    ReqData2 = wrq:set_resp_body(NewDoc, ReqData),
    {true, ReqData2, Ctx}.

to_json(ReqData, Ctx) ->

    {[], ReqData, Ctx}.

to_html(ReqData, Ctx) ->
    case we_utils:maybe_fetch_object(Ctx, "user.html") of
        {true, NewCtx} ->
            Body = NewCtx#context.response_body,
            {Body, ReqData, NewCtx};
        {false, NewCtx} ->
            {error, ReqData, NewCtx}
    end.

from_json(ReqData, Ctx) ->
    JsonDoc = wrq:req_body(ReqData),
    User = ?json_to_record(user, JsonDoc),
    io:format("~p~n", [User]),
    %NewDoc = we_db:update(Ctx#ctx.db, ID, Doc),
    %ReqData2 = wrq:set_resp_body(NewDoc, ReqData),
    {true, ReqData, Ctx}.
