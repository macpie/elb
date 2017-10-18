-module(worker_test).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/0, start_link/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([
    init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?SERVER, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_Args) ->
    {'ok', #state{}}.

handle_call(_Msg, _From, State) ->
    {'reply', 'ok', State}.

handle_cast(_Msg, State) ->
    {'noreply', State}.

handle_info({Message, From}, State) ->
    elb:reply(From, Message),
    {'noreply', State};
handle_info(_Msg, State) ->
    {'noreply', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, _State) ->
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
