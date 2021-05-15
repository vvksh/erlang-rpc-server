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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

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
  % module here indicates the module which implemented gen_server behavior
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

%% This is an active TCP socket which will forward all messages to the process
%% These messages are delegated to handle_info/2 callback function
%% In this case, all rpc command messages from telnet will be passed here
handle_info({tcp, Socket, RawData}, State) ->
  do_rpc(Socket, RawData),
  RequestCount = State#state.request_count,
  {noreply, State#state{request_count = RequestCount + 1 }};

%% handles the timeout case during init which will wait for TCP connection on the socket
handle_info(timeout, #state{lsock = LSock} = State) ->
  {ok, _Sock} = gen_tcp:accept(LSock),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%======================================
%%% Internal functions
%%%======================================

%% @doc parses the command, executes it and sends the response back to the socket.
do_rpc(Socket, RawData) ->
  try
    % extract module, function and args
    {M, F, A} = split_out_mfa(RawData),
    Result = apply(M, F, A), % executes the command in the same process
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
  catch
      _Class:Err  ->
        gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
  end.


split_out_mfa(RawData) ->
  MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
  {match, [M, F, A]} =
      re:run(MFA,
      "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
      [{capture, [1,2,3], list}, ungreedy]),
    {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
  {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
  {ok, Args} = erl_parse:parse_term(Toks),
  Args.
