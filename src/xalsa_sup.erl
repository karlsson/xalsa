%%%-------------------------------------------------------------------
%%% @author Mikael Karlsson <mikael.karlsson@creado.se>
%%% @copyright (C) 2019, Mikael Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 23 May 2019 by Mikael Karlsson <mikael.karlsson@creado.se>
%%%-------------------------------------------------------------------
-module(xalsa_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_xalsa_server/2,
	 stop_xalsa_server/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
		      {error, {already_started, Pid :: pid()}} |
		      {error, {shutdown, term()}} |
		      {error, term()} |
		      ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
start_xalsa_server(Name, Options) ->
    supervisor:start_child(?SERVER, 
			   xalsa_server_child_spec(Name, Options)).
stop_xalsa_server(Name) ->
    supervisor:terminate_child(?SERVER, Name),
    supervisor:delete_child(?SERVER, Name).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
		  {ok, {SupFlags :: supervisor:sup_flags(),
			[ChildSpec :: supervisor:child_spec()]}} |
		  ignore.
init([]) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    AChild = #{id => 'Xalsa.Manager',
	       start => {xalsa_manager, start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [xalsa_manager]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
xalsa_server_child_spec(Name, Options) ->
    #{id => Name,
      start => {xalsa_server, start_link, [Name, Options]},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [xalsa_server]}.
