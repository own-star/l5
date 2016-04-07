%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(CACHE, "l5_emo_server").

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req2),
	{ok, Req3} = maybe_echo(Method, HasBody, Req2),
%{ok, Req3} = cowboy_req:reply(200, [
%		{<<"content-type">>, <<"text/plain">>}
%%	], list_to_binary(Req2), Req),
%	], <<"Hello world!", Method/binary, 10>>, Req),
	{ok, Req3, State}.

maybe_echo(<<"POST">>, true, Req) ->
%	{ok, PostVals, Req2} = cowboy_req:body_qs(Req),
%	Echo = proplists:get_value(<<"action">>, PostVals),
	{Args, Req1} = cowboy_req:qs_vals(Req),
	{ok, Bin, Req2} = cowboy_req:body(Req1),
	Val = jsx:decode(Bin),
	io:format("Args: ~p~n, Vals: ~p~n", [Args, Val]),
	Action = proplists:get_value(<<"action">>, Val),
%	Key = proplists:get_value(<<"key">>, Val),
%	Value = proplists:get_value(<<"value">>, Val),
%	Ract(Action, Val),
%	Json = jsx:encode(Response),
%	{ok, Req3} = cowboy_req:reply(200, [], Response, Req2),
%	{ok, Req3, Req};


	act(Action, Val, Req2);
maybe_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

act(undefined, _Val, Req) ->
	cowboy_req:reply(400, [], <<"Missing action parameter.">>, Req);
act(<<"insert">>, Val, Req) ->
	Action = binary_to_list(proplists:get_value(<<"action">>, Val)),
	Key = binary_to_list(proplists:get_value(<<"key">>, Val)),
	Value = binary_to_list(proplists:get_value(<<"value">>, Val)),
	io:format("Action: ~p, Key: ~p, Value ~p~n", [Action, Key, Value]),
	Response = l5_emo_server:put_emo({Key,Value},60),
	io:format("Response: ~p~n", [Response]),
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], atom_to_binary(Response,utf8), Req).



terminate(_Reason, _Req, _State) ->
	ok.
