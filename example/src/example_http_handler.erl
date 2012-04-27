-module(example_http_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Session, _} = cowboy_session:from_req(Req),
    case Session of
    	undefined ->
	    {ok, Req2} = cowboy_http_req:reply(200, [], <<"No session">>, Req);
	_ ->
	    SessionId = cowboy_session_server:session_id(Session),
	    SessionState = cowboy_session_server:command(Session, return_field),
	    {ok, Req2} = cowboy_http_req:reply(200, [], <<"Session ID: ", SessionId/binary, "\nState: ", SessionState/binary>>, Req)
    end,		
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
