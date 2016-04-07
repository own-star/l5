%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(emo_server_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	Procs = [{l5_emo_server, {l5_emo_server, start, [[{drop_interval,300}]]},
		permanent, 5000, worker, [l5_emo_server]}],
	{ok, {{one_for_one, 10, 10}, Procs}}.
