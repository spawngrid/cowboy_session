-module(cowboy_session).
-export([on_request/1, on_request/2, on_response/1, on_response/2, from_req/1]).
-include_lib("cowboy/include/http.hrl").
-compile({parse_transform, seqbind}).

%% WARNING: these methods are convenient but will likely perform
%% much worse because of the constant need to call the supervisor
%% (effectively, a bottleneck)
on_request(Req)  ->
	[{{cowboy_session_server_sup, Handler}, _, _, _}] = supervisor:which_children(cowboy_session_sup),
    on_request(Req, Handler).

on_response(Req)  ->
	[{{cowboy_session_server_sup, Handler}, _, _, _}] = supervisor:which_children(cowboy_session_sup),
    on_request(Req, Handler).
%%%

on_request(Req@, Handler) ->
	CookieName = Handler:cookie_name(),
    Session = case cowboy_http_req:cookie(CookieName, Req@) of
                  {undefined, Req@} -> undefined;
                  {B, Req@} when is_binary(B) -> B
              end,
    SessionName = Handler:session_name(Session),
    case gproc:lookup_local_name({cowboy_session, SessionName}) of
    	undefined ->
    	    %% create a new session if necessary
    	    case Session of
    	    	undefined ->
    	    	   NewSession = Handler:generate();
    	    	_ ->
    	    	   NewSession = Handler:validate(Session)
    	    end,
		    {ok, Pid} = supervisor:start_child(cowboy_session_server_sup, [NewSession, Handler:session_name(NewSession)]);
		Pid ->
            ok
	end,
    cowboy_session_server:touch(Pid),
    HandlerState = cowboy_session_server:handler_state(Pid),
    Options = Handler:cookie_options(HandlerState),
    ResponseSession = cowboy_session_server:session_id(Pid),
    {ok, Req@} = cowboy_http_req:set_resp_cookie(CookieName, ResponseSession, Options, Req@),
    Req@#http_req{ meta = [{cowboy_session, Pid}|Req@#http_req.meta]}.

on_response(Req@, Handler) ->
    {Pid, Req@} = from_req(Req@),
	CookieName = Handler:cookie_name(),
    HandlerState = cowboy_session_server:handler_state(Pid),
    Options = Handler:cookie_options(HandlerState),
    ResponseSession = cowboy_session_server:session_id(Pid),
    {ok, Req@} = cowboy_http_req:set_resp_cookie(CookieName, ResponseSession, Options, Req@),
    Req@.

from_req(Req) ->
    cowboy_http_req:meta(cowboy_session, Req).

