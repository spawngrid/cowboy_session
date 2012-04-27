-module(example_http_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Session, _} = cowboy_http_req:meta(cowboy_session, Req),
    case Session of
    	undefined ->
		    {ok, Req2} = cowboy_http_req:reply(200, [], <<"No session">>, Req);
		_ ->
		    {ok, Req2} = cowboy_http_req:reply(200, [], cowboy_session_server:session_id(Session), Req)
    end,		
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.