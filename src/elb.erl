%% @author Pierre Defebvre
%% @doc
%% == Basic Load Balancer ==
%% Create workers and Round Robin messages to them
%% @end
%%%-------------------------------------------------------------------
-module(elb).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/1, start/1, stop/1
    ,call/2
    ,cast/2
    ,subscribe/1, subscribe/2
    ,unsubscribe/1, unsubscribe/2
    ,broadcast/2
    ,get_workers/1
    ,reply/2
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

-record(state, {
    worker
    ,worker_args
    ,size
    ,workers
    ,restart
}).

-type server_ref() :: atom() | {atom(), atom()} | {'global', atom()} | {'via', atom(), atom()} | pid().

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link(list()) -> {'ok', pid()} | 'ignore' | {'error', any()}.
start_link(Args) ->
    gen_server:start_link(?SERVER, Args, []).

-spec start(list()) -> {'ok', pid()} | 'ignore' | {'error', any()}.
start(Args) ->
    gen_server:start(?SERVER, Args, []).

-spec stop(server_ref()) -> 'ok'.
stop(Server) ->
    gen_server:stop(Server).

%%--------------------------------------------------------------------
%% @doc
%% Send message to worker `{Message, From}' with expectation for reponse from Worker.
%% Worker should reply via `{@module}:reply(From, Reply)'
%% @end
%%--------------------------------------------------------------------
-spec call(server_ref(), any()) -> any().
call(Server, Message) ->
    gen_server:call(Server, {'call', Message}).

%%--------------------------------------------------------------------
%% @doc
%% Send async message to worker
%% @end
%%--------------------------------------------------------------------
-spec cast(server_ref(), any()) -> 'ok'.
cast(Server, Message) ->
    gen_server:cast(Server, {'cast', Message}).

%%--------------------------------------------------------------------
%% @doc
%% Add `self()' to worker pool
%% @end
%%--------------------------------------------------------------------
-spec subscribe(server_ref()) -> 'ok'.
subscribe(Server) ->
    subscribe(Server, erlang:self()).

%%--------------------------------------------------------------------
%% @doc
%% Add `pid()' to worker pool
%% @end
%%--------------------------------------------------------------------
-spec subscribe(server_ref(), pid()) -> 'ok'.
subscribe(Server, PID) ->
    gen_server:call(Server, {'subscribe', PID}).

%%--------------------------------------------------------------------
%% @doc
%% Remove `self()' from worker pool
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(server_ref()) -> 'ok'.
unsubscribe(Server) ->
    unsubscribe(Server, erlang:self()).

%%--------------------------------------------------------------------
%% @doc
%% Remove `pid()' from worker pool
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(server_ref(), pid()) -> 'ok'.
unsubscribe(Server, PID) ->
    gen_server:call(Server, {'unsubscribe', PID}).

%%--------------------------------------------------------------------
%% @doc
%% Broadcast message workers
%% @end
%%--------------------------------------------------------------------
-spec broadcast(server_ref(), any()) -> 'ok'.
broadcast(Server, Message) ->
    gen_server:cast(Server, {'broadcast', Message}).

%%--------------------------------------------------------------------
%% @doc
%% Return list of workers
%% @end
%%--------------------------------------------------------------------
-spec get_workers(server_ref()) -> {'ok', [pid(), ...]}.
get_workers(Server) ->
    gen_server:call(Server, 'get_workers').

%%--------------------------------------------------------------------
%% @doc
%% Abstract `gen_server:reply'
%% @end
%%--------------------------------------------------------------------
-spec reply({pid(), reference()}, any()) -> any().
reply(From, Message) ->
    gen_server:reply(From, Message).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Args) ->
    process_flag('trap_exit', 'true'),

    Worker = proplists:get_value('worker', Args),
    WorkerArgs = proplists:get_value('worker_args', Args, []),
    Size = proplists:get_value('size', Args, 1),
    Restart = proplists:get_value('restart', Args, 'true'),

    CreateWorker = fun() -> start_worker(Worker, WorkerArgs) end,
    Workers = [CreateWorker() || _ <- lists:seq(1, Size)],

    State = #state{
        worker=Worker
        ,worker_args=WorkerArgs
        ,size=Size
        ,workers=Workers
        ,restart=Restart
    },
    {'ok', State}.

handle_call({'call', _Message}, _From, #state{workers=[]}=State) ->
    {'reply', {'error', 'empty'}, State};
handle_call({'call', Message}, From, #state{workers=[Worker|Tail]}=State) ->
    Worker ! {Message, From},
    {'noreply', State#state{workers=append(Tail, Worker)}};
handle_call({'subscribe', Pid}, _From, #state{workers=Workers}=State) ->
    {'reply', 'ok', State#state{workers=append(Workers, Pid)}};
handle_call({'unsubscribe', Pid}, _From, #state{workers=Workers}=State) ->
    {'reply', 'ok', State#state{workers=lists:delete(Pid, Workers)}};
handle_call('get_workers', _From, #state{workers=Workers}=State) ->
    {'reply', {'ok', Workers}, State};
handle_call(_Msg, _From, State) ->
    {'reply', 'ok', State}.

handle_cast({_Type, _Msg}, #state{workers=[]}=State) ->
    {'noreply', State};
handle_cast({'cast', Message}, #state{workers=[Worker|Tail]}=State) ->
    Worker ! Message,
    {'noreply', State#state{workers=append(Tail, Worker)}};
handle_cast({'broadcast', Message}, #state{workers=Workers}=State) ->
    lists:foreach(
        fun(Worker) ->
            Worker ! Message
        end
        ,Workers
    ),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

handle_info({'EXIT', Pid, _Reason}, #state{restart='true'
                                          ,worker=Mod
                                          ,worker_args=WorkerArgs
                                          ,workers=Tail}=State) ->
    {'ok', Worker} = erlang:apply(Mod, 'start_link', WorkerArgs),
    Workers = append(lists:delete(Pid, Tail), Worker),
    {'noreply', State#state{workers=Workers}};
handle_info({'EXIT', Pid, _Reason}, #state{workers=Tail}=State) ->
    Workers = lists:delete(Pid, Tail),
    {'noreply', State#state{workers=Workers}};
handle_info({'EXIT', Pid, 'normal'}, #state{workers=Tail}=State) ->
    Workers = lists:delete(Pid, Tail),
    {'noreply', State#state{workers=Workers}};
handle_info(_Msg, State) ->
    {'noreply', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, _State) ->
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec append(list(), any()) -> list().
append(List, Elem) ->
    lists:append(List, [Elem]).

-spec start_worker(atom() | {atom(), atom()}, list()) -> pid().
start_worker({Module, Fun}, Args) ->
    {'ok', Pid} = erlang:apply(Module, Fun, Args),
    Pid;
start_worker(Module, Args) ->
    {'ok', Pid} = erlang:apply(Module, 'start_link', Args),
    Pid.
