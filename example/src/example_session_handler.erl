-module(example_session_handler).
-extends(cowboy_session_default_handler).
-export([validate/1, init/2, handle/3]).

-record(my_state, { field }).

validate(_Session) ->
   ?MODULE:generate().

init(_Session, _SessionName) ->
    #my_state{ field = <<"data">> }.

handle(return_field, _Session, #my_state{ field = Field } = State) ->
    {Field, State}.
