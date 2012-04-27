-module(example).
-export([start/0]).


start() ->
    start_app(example).

start_app(App) ->
    case application:start(App) of
        {error, {not_started, Dep}} ->
            start_app(Dep),
            start_app(App);
        Other ->
            Other
    end.