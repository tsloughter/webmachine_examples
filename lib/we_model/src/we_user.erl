-module(we_user).

-export([new/3,
         username/1,
         first_name/1,
         last_name/1]).

-include("we_user.hrl").

-spec new(binary(), binary(), binary()) -> record(user).
new(Username, FirstName, LastName) ->
    #user{username=Username, first_name=FirstName, last_name=LastName}.

-spec username(record(user)) -> binary().
username(User) ->
    User#user.username.

-spec first_name(record(user)) -> binary().
first_name(User) ->
    User#user.first_name.

-spec last_name(record(user)) -> binary().
last_name(User) ->
    User#user.last_name.
