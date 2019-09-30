%%%-------------------------------------------------------------------
%%% @author Mikael Karlsson <mikael.karlsson@creado.se>
%%% @copyright (C) 2019, Mikael Karlsson
%%% @doc
%%%
%%% @end
%%% Created: 16 Apr 2019 by Mikael Karlsson <mikael.karlsson@creado.se>
%%%-------------------------------------------------------------------
-module(xalsa_server).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_continue/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-record(state,
        {
         name :: atom(),
         handle :: reference(),
         rate :: pos_integer(),
         no_of_channels :: pos_integer(),
         buffer_size :: pos_integer(),
         period_size :: pos_integer(),
         period_time :: pos_integer(),
         buffers :: tuple(), % One buffer per channel
         dtime = 0 :: pos_integer(),
         max_map_size = 0 :: pos_integer()
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(Name::atom(), Args::map()) ->
                        {ok, Pid :: pid()} |
                        {error, Error :: {already_started, pid()}} |
                        {error, Error :: term()} |
                        ignore.
start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args#{name => Name}, []).

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
                              {stop, Reason :: term()} |
                              ignore.
init(#{name := AtomName, rate := Rate,
       no_of_channels := Channels, period_size := PeriodSize}) ->
    process_flag(trap_exit, true),
    Name = atom_to_list(AtomName),
    Handle = xalsa_pcm:open_handle(Name),
    Conf = xalsa_pcm:set_params(Handle, Channels, Rate, PeriodSize),
    logger:info("~p - opened device ~p rate: ~p channels: ~p",[?MODULE, AtomName, Rate, Channels]),
    {Rate, Channels, BufferSize, PeriodSize} = Conf,
    PeriodTime = round(PeriodSize * 1000 / Rate),
    Bufs = erlang:make_tuple(Channels, #{}),
    {ok,
     #state{name = AtomName, handle = Handle,
            rate = Rate, no_of_channels = Channels,
            buffer_size = BufferSize, period_size = PeriodSize,
            period_time = PeriodTime, buffers = Bufs},
     {continue, {Handle, PeriodSize}}}.

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
handle_call(max_mix_time, _From, State) ->
    {reply, State#state.dtime, State};
handle_call(max_map_size, _From, State) ->
    {reply, State#state.max_map_size, State};
handle_call(clear_max_mix_time, _From, State) ->
    {reply, ok, State#state{dtime = 0}};
handle_call(clear_max_map_size, _From, State) ->
    {reply, ok, State#state{max_map_size = 0}};
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
%% @private
%% @doc
%% Handling continue call - start the alsa pcm driver callback cycling
%% @end
%%--------------------------------------------------------------------
-spec handle_continue(Continue :: term(), State :: term()) ->
                             {noreply, NewState :: term()} |
                             {noreply, NewState :: term(), Timeout :: timeout()} |
                             {noreply, NewState :: term(), hibernate} |
                             {stop, Reason :: term(), NewState :: term()}.
handle_continue({Handle, PeriodSize}, State) ->
    process_flag(priority, high),
    0 = xalsa_pcm:add_async_handler(Handle, self()),
    0 = xalsa_pcm:prepare(Handle),
    {_NewBufs, Frames} = prepare_bufs(State),
    xalsa_pcm:writen(Handle, Frames, PeriodSize),
    xalsa_pcm:writen(Handle, Frames, PeriodSize),
    0 = xalsa_pcm:start(Handle),
    {noreply, State}.

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
handle_info(pcm_ready4write,
            State = #state{name = Name, handle = Handle, period_size = Size,
                           period_time = PT, dtime = Dtime}) ->
    T1 = sysnow(),
    {NewBufs, Frames} = prepare_bufs(State),
    ErrNo = xalsa_pcm:writen(Handle, Frames, Size),
    case xalsa_pcm:errno(ErrNo) of
        Size ->
            ok;
        eagain ->
            xalsa_pcm:writen(Handle, Frames, Size);
        epipe ->
            logger:warning("~p - ~p epipe",[?MODULE, Name]),
            xalsa_pcm:recover(Handle, ErrNo),
            xalsa_pcm:writen(Handle, Frames, Size),
            xalsa_pcm:writen(Handle, Frames, Size);
        A ->
            logger:error("~p - Wrong Size, got ~p but expected ~p", [?MODULE, A,Size])
    end,
    garbage_collect(),
    T2 = sysnow(),
    {noreply, State#state{buffers = NewBufs, dtime = max(Dtime,T2 - T1)}, 4 * PT};

handle_info({frames,{Pid, Channel, PidBuf, Notify}},
	    State = #state{buffers = Bufs, max_map_size = MSize}) when
      is_pid(Pid), Channel =< State#state.no_of_channels ->
    OldBuf = element(Channel, Bufs),
    NewBuf =
	case OldBuf of
            #{Pid := {_, Frames}} when is_binary(Frames)->
                OldBuf#{Pid := {Notify, <<Frames/binary, PidBuf/binary>>}};
            OldBuf ->
                OldBuf#{Pid => {Notify, PidBuf}}
        end,
    NewBufs = setelement(Channel, Bufs, NewBuf),
    {noreply, State#state{buffers = NewBufs,
                          max_map_size = max(MSize, maps:size(NewBuf))}};

handle_info(timeout, State = #state{handle = Handle, period_size = Size,
                                    period_time = PT}) ->
    Avail = xalsa_pcm:avail_update(Handle),
    logger:warning("~p, Timout waiting for callback",[?MODULE]),
    NewBufs =
        if
            Avail >= Size ->
                {NewBufs1, Frames} = prepare_bufs(State),
                xalsa_pcm:writen(Handle, Frames, Size),
                if
                    Avail - Size >= Size ->
                        xalsa_pcm:writen(Handle, Frames, Size);
                    true ->
                        ok
                end,
                NewBufs1;
            true ->
                State#state.buffers
        end,
    {noreply, State#state{buffers = NewBufs}, 4 * PT};

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
terminate(_Reason, #state{name = Name, handle = Handle,
                          dtime = Dtime, max_map_size = MSize}) ->
    logger:info("~p - Closing ~p, max mix time: ~p µs, max_map_size: ~p",[?MODULE, Name, Dtime, MSize]),
    xalsa_pcm:close_handle(Handle).

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
prepare_bufs(#state{period_size = Size, no_of_channels = Channels,
                    buffers = Bufs}) when Size > 0 ->
    prepare_bufs(1, Size, Bufs, Channels, []).

prepare_bufs(Channel, Size, Bufs, Channels, Acc) when Channel =< Channels ->
    Buf = element(Channel, Bufs),
    {Frames, NewBuf} = xalsa_pcm:sum_map(Buf, Size),
    NewBufs = setelement(Channel, Bufs, NewBuf),
    prepare_bufs(Channel+1, Size, NewBufs, Channels, [Frames | Acc]);
prepare_bufs(_, _, Bufs, _, Acc) ->
    {Bufs, lists:reverse(Acc)}.

sysnow() ->
    %% microseconds past epoc
    erlang:system_time(microsecond).
