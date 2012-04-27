-module(cowboy_session_default_handler).
-export([cookie_name/0, cookie_options/0, generate/0, stop/1, validate/1, handle/2]).

cookie_name() ->
   <<"_session">>.

cookie_options() ->
   [{path, <<"/">>}].

generate() ->
   ossp_uuid:make(v4, text).

validate(Session) ->
   Session.

stop(_Session) ->
   ok.

handle(_Command, _Session) ->
   ok.