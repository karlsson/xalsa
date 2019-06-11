%%%-------------------------------------------------------------------
%%% @author Mikael Karlsson <mikael.karlsson@creado.se>
%%% @copyright (C) 2019, Mikael Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 23 May 2019 by Mikael Karlsson <mikael.karlsson@creado.se>
%%%-------------------------------------------------------------------
-module(xalsa_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start_phase/3, stop/1, prep_stop/1,
	 config_change/3]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
-spec start(StartType :: normal |
			 {takeover, Node :: node()} |
			 {failover, Node :: node()},
	    StartArgs :: term()) ->
		   {ok, Pid :: pid()} |
		   {ok, Pid :: pid(), State :: term()} |
		   {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    case xalsa_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% top supervisor of the tree.
%% Starts an application with included applications, when
%% synchronization is needed between processes in the different
%% applications during startup.%% @end
%%--------------------------------------------------------------------
-spec start_phase(Phase :: atom(),
		  StartType :: normal |
			       {takeover, Node :: node()} |
			       {failover, Node :: node()},
		  PhaseArgs :: term()) -> ok | {error, Reason :: term()}.
start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec stop(State :: term()) -> any().
stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
%% @end
%%--------------------------------------------------------------------
-spec prep_stop(State :: term()) -> NewState :: term().
prep_stop(State) ->
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by an application after a code replacement,
%% if the configuration parameters have changed.
%% @end
%%--------------------------------------------------------------------
-spec config_change(Changed :: [{Par :: atom(), Val :: term()}],
		    New :: [{Par :: atom(), Val :: term()}],
		    Removed :: [Par :: atom()]) -> ok.
config_change(_Changed, _New, _Removed) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
