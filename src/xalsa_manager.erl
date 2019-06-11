%%%-------------------------------------------------------------------
%%% @author Mikael Karlsson <mikael.karlsson@creado.se>
%%% @copyright (C) 2019, Mikael Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 23 May 2019 by Mikael Karlsson <mikael.karlsson@creado.se>
%%%-------------------------------------------------------------------
-module(xalsa_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 period_size/0,
	 rate/0,
	 pcms/0,
	 no_of_channels/0,
	 max_mix_time/0,
	 clear_max_mix_time/0,
	 get_id/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_continue/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-type rate() :: 44100 | 48000 | 96000 | 192000.
-type period_size() :: 224 | 240 | 480 | 960.

-record(state, {
		conf_rate :: rate(),
		period_size :: period_size(),
		channels = []
	       }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
		      {error, Error :: {already_started, pid()}} |
		      {error, Error :: term()} |
		      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
period_size() ->
    gen_server:call(?SERVER, ?FUNCTION_NAME).
rate() ->
    gen_server:call(?SERVER, ?FUNCTION_NAME).
pcms() ->
    gen_server:call(?SERVER, ?FUNCTION_NAME).
no_of_channels() ->
    gen_server:call(?SERVER, ?FUNCTION_NAME).
max_mix_time() ->
    gen_server:call(?SERVER, ?FUNCTION_NAME).
clear_max_mix_time() ->
    gen_server:call(?SERVER, ?FUNCTION_NAME).
get_id(Channel) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Channel}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
				  {ok, State :: #state{}, {continue,[]}} |
			      {stop, Reason :: term()} |
			      ignore.
init([]) ->
    process_flag(trap_exit, true),
    Rate =
	case application:get_env(xalsa, rate) of
	    {ok, A} -> A;
	    _ -> 44100
	end,

    %% Prefer to set no of frames per period to multiple of 8
    %% (or even 64). Current setting gives a time of 5 ms.
    PeriodSize =
	case Rate of
	    44100 -> 224;
	    48000 -> 240;
	    96000 -> 480;
        192000-> 960
	end,
    {ok, #state{conf_rate = Rate, period_size = PeriodSize}, {continue,[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
			 {reply, Reply :: term(), NewState :: term()} |
			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
			 {reply, Reply :: term(), NewState :: term(), hibernate} |
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_call({get_id, Channel}, _From, State) ->
    Reply = get_local_id_channel(Channel, State#state.channels),
    {reply, Reply, State};
handle_call(period_size, _From, State) ->
    {reply, State#state.period_size, State};
handle_call(rate, _From, State) ->
    {reply, State#state.conf_rate, State};
handle_call(pcms, _From, State) ->
    {reply, State#state.channels, State};
handle_call(no_of_channels, _From, State) ->
    {reply, sum_channels(State#state.channels), State};
handle_call(max_mix_time, _From, State) ->
    {reply, max_mix_time(State#state.channels), State};
handle_call(clear_max_mix_time, _From, State) ->
    {reply, clear_max_mix_time(State#state.channels), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_continue(_Continue, #state{channels = GlobalChannels,
				  conf_rate = Rate,
				  period_size = PeriodSize} = State) ->
    NewChs =
	case application:get_env(xalsa, pcms) of
	    {ok, PCMS} ->
		start_servers(PCMS, Rate, PeriodSize, GlobalChannels);
	    _ ->
		GlobalChannels
	end,
    {noreply, State#state{channels = NewChs}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
				      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_servers([{AtomName, NoChannels}|T], Rate, PeriodSize, GlobalChannels) ->
    {ok, _Pid} = xalsa_sup:start_xalsa_server(AtomName,
					    #{rate => Rate,
					      period_size => PeriodSize,
					      no_of_channels => NoChannels}),
    NewGlobalChannels = add_channel(GlobalChannels, {AtomName, NoChannels}),
    start_servers(T, Rate, PeriodSize, NewGlobalChannels);
start_servers([], _, _, GlobalChannels) ->
    GlobalChannels.

add_channel(Channels, {Id, NoOfLocalChannels}) ->
    Channels ++ [{Id, NoOfLocalChannels}].

get_local_id_channel(ChNo, Channels) ->
    get_local_id_channel(ChNo, Channels, 0).

get_local_id_channel(ChNo, [{_Id, Channels}| T], Count)
  when Count + Channels < ChNo ->
    get_local_id_channel(ChNo, T, Count + Channels);
get_local_id_channel(ChNo, [{Id, _Channels}| _T], Count) ->
    {Id, ChNo - Count};
get_local_id_channel(_, [], _) ->
    {error, channel_number_to_large}.

sum_channels(Chs) ->
    sum_channels(Chs,0).
sum_channels([{_, N0}|T], N) ->
    sum_channels(T, N + N0);
sum_channels([],N) ->
    N.

max_mix_time([{Id,_}|T]) ->
    [gen_server:call(Id,max_mix_time)|max_mix_time(T)];
max_mix_time([]) ->
    [].

clear_max_mix_time([{Id,_}|T]) ->
    gen_server:call(Id,clear_max_mix_time),
    clear_max_mix_time(T);
clear_max_mix_time([]) ->
    ok.
