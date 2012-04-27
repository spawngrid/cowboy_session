-module(example_session_handler).
-extends(cowboy_session_default_handler).
-export([validate/1]).

validate(_Session) ->
   ?MODULE:generate().