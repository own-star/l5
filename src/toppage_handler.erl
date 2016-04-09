%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req2),
	{ok, Req3} = maybe_echo(Method, HasBody, Req2),
	{ok, Req3, State}.

maybe_echo(<<"POST">>, true, Req) ->
	{_Args, Req1} = cowboy_req:qs_vals(Req),
	{ok, Bin, Req2} = cowboy_req:body(Req1),
	Val = jsx:decode(Bin),
	Action = proplists:get_value(<<"action">>, Val),
	act(Action, Val, Req2);
maybe_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

act(undefined, _Val, Req) ->
	cowboy_req:reply(400, [], <<"Missing action parameter.">>, Req);
act(<<"insert">>, Val, Req) ->
	Key = binary_to_list(proplists:get_value(<<"key">>, Val)),
	Value = binary_to_list(proplists:get_value(<<"value">>, Val)),
	Response = l5_emo_server:put_emo({Key,Value},60),
	ResponseBin = atom_to_binary(Response,utf8),
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], <<ResponseBin/binary,10>>, Req);
act(<<"lookup_by_date">>, Val, Req) ->
	Date_from = proplists:get_value(<<"date_from">>, Val),
	Date_to = proplists:get_value(<<"date_to">>, Val),
	{ok, Response} = l5_emo_server:get_by_date(todate(Date_from,<<>>,[],[]),todate(Date_to,<<>>,[],[])),
	ResponseBin = tobin(Response,<<>>),
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], <<ResponseBin/binary,10>>, Req);
act(<<"lookup">>, Val, Req) ->
	Key = binary_to_list(proplists:get_value(<<"key">>, Val)),
	Response = l5_emo_server:get_emo(Key),
	ResponseBin = atom_to_binary(Response,utf8),
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], <<ResponseBin/binary,10>>, Req).

todate(<<"/",Rest/binary>>,Acc,[],_AccT) ->
	todate(Rest,<<>>,[binary_to_integer(Acc)],[]);
todate(<<"/",Rest/binary>>,Acc,AccD,_AccT) ->
	todate(Rest,<<>>,[binary_to_integer(Acc)|AccD],[]);
todate(<<" ",Rest/binary>>,Acc,AccD,_AccT) ->
	todate(Rest,<<>>,[binary_to_integer(Acc)|AccD],[]);
todate(<<":",Rest/binary>>,Acc,AccD,AccT) ->
	todate(Rest,<<>>,AccD,[binary_to_integer(Acc)|AccT]);
todate(<<X,Rest/binary>>,Acc,AccD,AccT) ->
	todate(Rest,<<Acc/binary,X>>,AccD,AccT);
todate(<<>>,Acc,AccD,AccT) ->
	{list_to_tuple(lists:reverse(AccD)), list_to_tuple(lists:reverse([binary_to_integer(Acc)|AccT]))}.

tobin([{Name,State}|T],<<>>) ->
	BinN = list_to_binary(Name),
	BinS = list_to_binary(State),
	tobin(T,<<"{",BinN/binary,",",BinS/binary,"}">>);
tobin([{Name,State}|T],Acc) ->
	BinN = list_to_binary(Name),
	BinS = list_to_binary(State),
	tobin(T,<<Acc/binary,",{",BinN/binary,",",BinS/binary,"}">>);
tobin([],Acc) ->
	<<"[",Acc/binary,"]">>.

terminate(_Reason, _Req, _State) ->
	ok.
