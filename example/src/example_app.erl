-module(example_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start_cowboy(),
    example_sup:start_link().

stop(_State) ->
    ok.

%% Internal

start_cowboy() ->
   Dispatch = [
                {'_', [{'_', example_http_handler, []}]}
               ],
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(tendita_http_listener, 100,
                          cowboy_tcp_transport, [{port, 8888}],
                          cowboy_http_protocol, [{dispatch, Dispatch},
                                                 {onrequest, fun cowboy_session:on_request/1}
                                                ]
                         ).
