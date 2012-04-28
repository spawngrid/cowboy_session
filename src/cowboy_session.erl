-module(cowboy_session).
-export([on_request/1, on_request/2, from_req/1]).
-include_lib("cowboy/include/http.hrl").
-compile({parse_transform, seqbind}).

%% WARNING: this method is convenient but will likely perform
%% much worse because of the constant need to call the supervisor
%% (effectively, a bottleneck)
on_request(Req)  ->
	[{{cowboy_session_server_sup, Handler}, _, _, _}] = supervisor:which_children(cowboy_session_sup),
    on_request(Req, Handler).

on_request(Req@, Handler) ->
	CookieName = Handler:cookie_name(),
    Session = case cowboy_http_req:cookie(CookieName, Req@) of
                  {undefined, Req@} -> undefined;
                  {B, Req@} when is_binary(B) -> B
              end,
    case gproc:lookup_local_name({cowboy_session, Session}) of
    	undefined ->
    	    %% create a new session if necessary
    	    case Session of
    	    	undefined ->
    	    	   FinalSession = Handler:generate();
    	    	_ ->
    	    	   FinalSession = Handler:validate(Session)
    	    end,
		    {ok, Pid} = supervisor:start_child(cowboy_session_server_sup, [FinalSession]);
		Pid ->
		    FinalSession = Session
	end,
    cowboy_session_server:touch(Pid),
    HandlerState = cowboy_session_server:handler_state(Pid),
    Options = Handler:cookie_options(HandlerState),
    {ok, Req@} = cowboy_http_req:set_resp_cookie(CookieName, FinalSession, Options, Req@),
    Req@#http_req{ meta = [{cowboy_session, Pid}|Req@#http_req.meta]}.

from_req(Req) ->
    cowboy_http_req:meta(cowboy_session, Req).

