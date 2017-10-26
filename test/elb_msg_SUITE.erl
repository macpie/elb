-module(elb_msg_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0
    ,groups/0
    ,init_per_suite/1, end_per_suite/1
    ,init_per_testcase/2, end_per_testcase/2
]).

-export([
    broadcast/1
    ,cast/1
    ,call/1
]).

-define(DEFAULT_SIZE, 10).
-define(WORKER_NAME, 'worker_test').

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Running tests for this suite
%% @end
%%--------------------------------------------------------------------
all() ->
    [{'group', 'elb'}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Tests groups
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{'elb', [], ['broadcast', 'cast', 'call']}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special init config for suite
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    {'ok',ELB} = elb:start([
        {'worker', {?WORKER_NAME, 'start_link'}}
        ,{'size', ?DEFAULT_SIZE}
    ]),

    {'ok', Workers} = elb:get_workers(ELB),
    ?assertEqual(erlang:length(Workers), ?DEFAULT_SIZE),

    [{'elb', ELB} | Config].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special init config for test case
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    ELB = proplists:get_value('elb', Config),
    {'ok', Workers} = elb:get_workers(ELB),
    'ok' = trace_receive(Workers, ['receive']),
    [{'workers', ELB} | Config].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special end config for test case
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Workers = proplists:get_value('workers', Config),
    'ok' = untrace_receive(Workers, ['receive']),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special end config for suite
%% @end
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    ELB = proplists:get_value('elb', Config),
    elb:stop(ELB),
    'ok'.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
broadcast(Config) ->
    ELB = proplists:get_value('elb', Config),
    Message = "BROADCAST",
    elb:broadcast(ELB, Message),
    ?assert(rcv_loop(?DEFAULT_SIZE, Message)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
cast(Config) ->
    ELB = proplists:get_value('elb', Config),
    Message = "CAST",
    elb:cast(ELB, Message),
    ?assert(rcv_loop(1, Message)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
call(Config) ->
    ELB = proplists:get_value('elb', Config),
    Message = "CALL",
    ?assertEqual(elb:call(ELB, Message), Message).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
rcv_loop(0, _Message) -> 'true';
rcv_loop(Int, Message) ->
    receive
        {'trace', _Pid, 'receive', Message} ->
            rcv_loop(Int-1, Message);
        _Msg ->
            rcv_loop(Int, Message)
    after 1000 ->
        'false'
    end.

trace_receive([], _Types) -> 'ok';
trace_receive([Pid|Pids], Types) ->
    _ = erlang:trace(Pid, 'true', Types),
    trace_receive(Pids, Types).

untrace_receive([], _Types) -> 'ok';
untrace_receive([Pid|Pids], Types) ->
    _ = erlang:trace(Pid, 'false', Types),
    untrace_receive(Pids, Types).
