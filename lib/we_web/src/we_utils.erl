-module(we_utils).

-export([maybe_fetch_object/2,
         file_exists/2,
         get_full_path/2]).

-include("context.hrl").
-include_lib("kernel/include/file.hrl").

maybe_fetch_object(Context, Path) ->
    % if returns {true, NewContext} then NewContext has response_body
    case Context#context.response_body of
        undefined ->
            case file_exists(Context, Path) of
                {true, FullPath} ->
                    {ok, Value} = file:read_file(FullPath),
                    {true, Context#context{response_body=Value}};
                false ->
                    {false, Context}
            end;
        _Body ->
            {true, Context}
    end.

file_exists(Context, Path) ->
    FPath = get_full_path(Context, Path),
    case filelib:is_regular(filename:absname(FPath)) of
        true ->
            {true, FPath};
        false ->
            false
    end.

get_full_path(Context, Path) ->
    Root = Context#context.docroot,
    case mochiweb_util:safe_relative_path(Path) of
        undefined -> undefined;
        RelPath ->
            FullPath = filename:join([Root, RelPath]),
            case filelib:is_dir(FullPath) of
                true ->
                    filename:join([FullPath, "index.html"]);
                false ->
                    FullPath
            end
    end.
