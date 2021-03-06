-module(elb_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0
    ,groups/0
    ,init_per_suite/1, end_per_suite/1
    ,init_per_testcase/2, end_per_testcase/2
]).

-export([
    undef/1
    ,subscribe/1
    ,restart/1
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
    [{'elb', [], ['undef', 'subscribe', 'restart']}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special init config for suite
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special init config for test case
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special end config for test case
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special end config for suite
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    'ok'.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
undef(_Config) ->
    {'ok', ELB} = elb:start_link([]),

    ?assertEqual({'error', 'empty'}, elb:call(ELB, "Fail")),
    elb:stop(ELB).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
subscribe(_Config) ->
    {'ok', ELB} = elb:start_link([]),

    Cast = "CAST",
    CastPid = cast_loop(Cast),
    ?assertEqual('ok', elb:subscribe(ELB, CastPid)),
    ?assertEqual('ok', elb:cast(ELB, Cast)),
    ?assertEqual('ok', elb:unsubscribe(ELB, CastPid)),

    Call = "Call",
    CallPid = call_loop(Call),
    ?assertEqual('ok', elb:subscribe(ELB, CallPid)),
    ?assertEqual('ok', elb:call(ELB, Call)),
    ?assertEqual('ok', elb:unsubscribe(ELB, CallPid)),

    CastPid1 = cast_loop(Cast),
    CastPid2 = cast_loop(Cast),
    ?assertEqual('ok', elb:subscribe(ELB, CastPid1)),
    ?assertEqual('ok', elb:subscribe(ELB, CastPid2)),
    ?assertEqual('ok', elb:broadcast(ELB, Cast)),
    ?assertEqual('ok', elb:unsubscribe(ELB, CastPid1)),
    ?assertEqual('ok', elb:unsubscribe(ELB, CastPid2)),

    elb:stop(ELB).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
restart(_Config) ->
    {'ok', ELB} = elb:start_link([
        {'worker', 'worker_test'}
        ,{'size', 1}
        ,{'restart', true}
    ]),

    {'ok', [Pid|[]]} = elb:get_workers(ELB),
    ?assertEqual('ok', elb:call(ELB, 'ok')),
    gen_server:stop(Pid, 'wrong', 100),
    {'ok', [_Pid|[]]} = elb:get_workers(ELB),
    ?assertEqual('ok', elb:call(ELB, 'ok')),

    elb:stop(ELB).

cast_loop(Expected) ->
    erlang:spawn(
        fun() ->
            receive
                Msg -> ?assertEqual(Expected, Msg)
            after 1000 ->
                ct:fail("timeout")
            end
        end
    ).

call_loop(Expected) ->
    erlang:spawn(
        fun() ->
            receive
                {Msg, From} ->
                    ?assertEqual(Expected, Msg),
                    elb:reply(From, 'ok')
            after 1000 ->
                ct:fail("timeout")
            end
        end
    ).
