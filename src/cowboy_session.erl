-module(cowboy_session).
-export([on_request/1, from_req/1]).
-include_lib("cowboy/include/http.hrl").
-compile({parse_transform, seqbind}).

on_request(Req@)  ->
	[{{cowboy_session_server_sup, Handler}, _, _, _}] = supervisor:which_children(cowboy_session_sup),
	CookieName = Handler:cookie_name(),
	Options = Handler:cookie_options(),
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
    {ok, Req@} = cowboy_http_req:set_resp_cookie(CookieName, FinalSession, Options, Req@),
    Req@#http_req{ meta = [{cowboy_session, Pid}|Req@#http_req.meta]}.

from_req(Req) ->
    cowboy_http_req:meta(cowboy_session, Req).