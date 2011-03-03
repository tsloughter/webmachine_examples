-module(we_resource_static).

-export([init/1,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         provide_content/2]).

-include("context.hrl").

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").

init([]) ->
    {ok, App}= application:get_application(),
    PrivDir = code:priv_dir(App),
    {ok, #context{docroot=PrivDir}}.

allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

resource_exists(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    Path = we_utils:get_full_path(Ctx, wrq:disp_path(ReqData)),
    {[{webmachine_util:guess_mime(Path), provide_content}], ReqData, Ctx}.

provide_content(ReqData, Context) ->
    case we_utils:maybe_fetch_object(Context, wrq:disp_path(ReqData)) of
        {true, NewContext} ->
            Body = NewContext#context.response_body,
            {Body, ReqData, Context};
        {false, NewContext} ->
            {error, ReqData, NewContext}
    end.

