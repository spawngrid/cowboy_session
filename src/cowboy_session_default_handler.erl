-module(cowboy_session_default_handler).
-export([cookie_name/0, cookie_options/1, generate/0, stop/2, validate/1, handle/3, init/1]).

cookie_name() ->
   <<"_session">>.

cookie_options(_HandlerState) ->
   [{path, <<"/">>}].

generate() ->
   ossp_uuid:make(v4, text).

validate(Session) ->
   Session.

-spec init(binary()) -> any().
init(_Session) ->
    undefined.

-spec stop(binary(), any()) -> any().
stop(_Session, _State) ->
    ok.

-spec handle(any(), binary(), any()) -> {any(), any()}.
handle(_Command, _Session, _State) ->
   ok.
