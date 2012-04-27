-module(example_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").


%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([]) ->
    #one_for_one{
      children = [
      				#worker{ 
      					id = example_session,
      					restart = permanent,
      					start_func = {cowboy_session_sup, start_link, [example_session_handler]}
      				}
                 ]
     }.

