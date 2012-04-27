-module(cowboy_session_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").


%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    start_link(cowboy_session_default_handler).

start_link(Handler) ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, [Handler]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([Handler]) ->
    #one_for_one{
      children = [
                  #simple_one_for_one{
                           id = {cowboy_session_server_sup, Handler},
                           registered = cowboy_session_server_sup,
                           children = [
	                           #worker{       
		                           id = cowboy_session_server,
		                           restart = transient,
		                           start_func = {cowboy_session_server, start_link, [Handler]}
		                        }
	                        ]
                         }
                 ]
     }.

