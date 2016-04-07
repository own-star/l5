%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(emo_server_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", toppage_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 10, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
	emo_server_sup:start_link().

stop(_State) ->
	ok.
