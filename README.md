# Erlang Load Balancer

[![CircleCI](https://img.shields.io/circleci/project/github/macpie/elb.svg?label=Build)](https://circleci.com/gh/macpie/elb)
[![Hex.pm](https://img.shields.io/hexpm/l/elb.svg?label=License)](https://github.com/macpie/elb/blob/master/LICENSE) /
[![Hex.pm](https://img.shields.io/hexpm/v/elb.svg)](https://hex.pm/packages/elb)
[![Hex.pm](https://img.shields.io/hexpm/dt/elb.svg?label=Downloads)](https://hex.pm/packages/elb) /
[![Website](https://img.shields.io/website-Up-Down-brightgreen-red/http/shields.io.svg?label=EDoc)](https://macpie.github.io/elb/)

Simple message load balancer using round robin style.

## Usage

Start using `elb:start(Options)` or `elb:start_link(Options)`

### Options

```Erlang
[
    {'worker', {Module, Function}} % Or {worker, Module}. Default to start_link function
    ,{'worker_args', []} % Arguments to pass to worker. Default to []
    ,{'size', 10} % Number of worker to start. Default to 0
    ,{'restart', true} % Auto restart dead worker. Default to false
]
```

## Examples

### Simple Subscribe

```Erlang
1> l(elb).
{module,elb}

2> {'ok', P} = elb:start_link([{'size', 0}]).
{ok,<0.666.0>}

3> elb:subscribe(P).
ok

4> elb:cast(P, 'test').
ok

5> flush().
Shell got test
ok
```

### Simple Gen Server

```Erlang
{'ok',ELB} = elb:start([
    {'worker', {'worker_test', 'start_link'}}
    ,{'worker_args', []}
    ,{'size', 10}
    ,{'restart', 'true'}
]),

'ok' = elb:cast(ELB, "ASYNC Message"),
'ok' = elb:broadcast(ELB, "Broadcast Message to all workers"),
"Sync Message" = elb:call(ELB, "Sync Message"),
```

```Erlang
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

```

## Build

[Rebar3](http://www.rebar3.org)

## Test

`rebar3 ct -v`

### Tested via [CircleCI](https://circleci.com/gh/macpie/elb)

[![](https://img.shields.io/badge/Erlang-19-brightgreen.svg)](https://hub.docker.com/_/erlang/)
[![](https://img.shields.io/badge/Erlang-20-brightgreen.svg)](https://hub.docker.com/_/erlang/)

[![](https://img.shields.io/badge/Test-dialyzer-brightgreen.svg)]()
[![](https://img.shields.io/badge/Test-xref-brightgreen.svg)]()
[![](https://img.shields.io/badge/Test-eunit-brightgreen.svg)]()
[![](https://img.shields.io/badge/Test-ct-brightgreen.svg)]()
