%%%-------------------------------------------------------------------
%%% @author viveks
%%% @copyright (C) 2021, viveks.xyz
%%% @doc allows users to execute rpc commands over TCP
%%% @end
%%% Created : 13. May 2021 11:53 PM
%%%-------------------------------------------------------------------
-module(tr_server).
-author("viveks").
-behavior(gen_server).


%% API
-export([start_link/0, start_link/1, get_count/0, stop/0]).

%% gen_server callbacks
%% init/1 is a blocking call until the process is up and running
%% handle_call/3: callback for synchronous requests, handles messages sent using
%%                gen_server:call/2
%% handle_cast/2: callback for asynchronous requests, handles messages sent using
%%                gen_server:cast/2
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).



%%%==============
%%% API
%%%==============

%% --------------------------------
%% @doc Starts the server
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%%    Pid = pid()
%% @end
%% --------------------------------
start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @doc Calls start_link(Port) using the default port
%% @spec start_link() -> {ok, Pid}.
start_link() ->
  start_link(?DEFAULT_PORT).

%% --------------------------------
%% @doc returns number of requests made to this server
%% @spec get_count() -> {ok, Count}
%% where
%%    Count = integer()
%% @end
%% --------------------------------
get_count() ->
  gen_server:call(?SERVER, get_count).

%% --------------------------------
%% @doc stops the server
%% @spec stop() -> ok.
%% @end
%% --------------------------------
stop() ->
  gen_server:cast(?SERVER, stop).


%%%==============
%%% Callbacks
%%%==============

init([Port]) ->
  % create a listening socket which will accept incoming tcp connections
  % {active, true} tells gen_tcp to send any incoming TCP data as messages to your process
  {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
  % return {ok, process_state, timeout }
  {ok, #state{port=Port, lsock = LSock}, 0}. %  0 timeout will cause handling timeout right after

handle_call(get_count, _From, State) ->
  {reply, {ok, State#state.request_count}, State}.  % send reply, which is {ok, request_count} , new state is same

handle_cast(stop, State) ->
  {stop, normal, State}.  % send stop signal, reason normal, state is same