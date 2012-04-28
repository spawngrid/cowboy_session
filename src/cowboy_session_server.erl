-module(cowboy_session_server).

-behaviour(gen_server).

%% API
-export([start_link/2, command/2, handler_state/1, session_id/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          handler,
	  handler_state,
          session
         }).

%%%===================================================================
%%% API
%%%===================================================================

command(Server, Command) ->
    gen_server:call(Server, {command, Command}).

handler_state(Server) ->
    gen_server:call(Server, handler_state).

session_id(Server) ->
    gen_server:call(Server, session_id).

stop(Server) ->
	  gen_server:cast(Server, stop).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Handler, Session) ->
    gen_server:start_link(?MODULE, [Handler, Session], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Handler, Session]) ->
    gproc:add_local_name({cowboy_session, Session}),
    HandlerState = Handler:init(Session),
    {ok, #state{
       session = Session,
       handler_state = HandlerState,
       handler = Handler
      }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(session_id, _From, #state{ session = Session } = State) ->
    {reply, Session, State};
handle_call(handler_state, _From, #state{ handler_state = HandlerState } = State) ->
    {reply, HandlerState, State};
handle_call({command, Command}, _From, #state{ handler = Handler, handler_state = HandlerState, session = Session } = State) ->
    {Reply, HandlerState1} = Handler:handle(Command, Session, HandlerState),
    {reply, Reply, State#state{ handler_state = HandlerState1 }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, #state{ handler = Handler, handler_state = HandlerState, session = Session } = State) ->
    Handler:stop(Session, HandlerState),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
