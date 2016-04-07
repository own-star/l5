-module(l5_emo_server).
-export([start/1, stop/0, get_emo/1, put_emo/2, get_by_date/2, custodian/0]).

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
              terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start([{drop_interval, DrTime}]) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [{drop_interval, DrTime}], []);
start(_) ->
	io:format("Wrong arguments.~nUse ~p:start([{drop_interval,Sec}])~n", [?MODULE]).
stop()  -> gen_server:call(?MODULE, stop).

get_emo(Who) -> gen_server:call(?MODULE, {get, Who}).
put_emo({Who, Status}, Time) -> gen_server:call(?MODULE, {put, Who, Status, Time}).
get_by_date(FromDate, ToDate) -> gen_server:call(?MODULE, {get_by_date, FromDate, ToDate}).

get_by_date('$end_of_table',_,_,Acc) ->
	{ok, Acc};
get_by_date(Name, FromSec, ToSec, Acc) ->
	[{_, Status, Exp}] = ets:lookup(?MODULE, Name),
	if
		Exp >= FromSec,	Exp =< ToSec ->
			get_by_date(ets:next(?MODULE,Name),FromSec,ToSec,[{binary_to_list(Name), binary_to_list(Status)}|Acc]);
		true -> 
			get_by_date(ets:next(?MODULE,Name),FromSec,ToSec,Acc)
	end.

custodian() ->
	CTime = calendar:datetime_to_gregorian_seconds({date(), time()}),
	io:format("Current time in seconds: ~p~n",[CTime]),
	custodian(ets:first(?MODULE),CTime).

custodian('$end_of_table',_) ->
	clear_table_done;
custodian(Name,CTime) ->
	io:format("Current time in seconds: ~p Name: ~p~n",[CTime,Name]),
	NextName = ets:next(?MODULE, Name),
	[{_,_,Exp}] = ets:lookup(?MODULE,Name),
	if
	   	CTime > Exp ->
			ets:delete(?MODULE,Name);
		true -> true
	end,
	custodian(NextName,CTime).


init([{drop_interval, DrTime}]) ->
	ok = timer:start(),
	{ok, _TRef} = timer:apply_interval(DrTime*1000, l5_emo_server, custodian, []),
	{ok, ets:new(?MODULE,[set,public,named_table])}.

handle_call({get, Who}, _From, Tab) ->
	BName = list_to_binary(Who),
	CTime = calendar:datetime_to_gregorian_seconds({date(), time()}),
	Reply = case ets:lookup(Tab, BName) of
		[] -> no_tuple_found;
		[{_,_,Exp}] when CTime > Exp ->
			no_tuple_found;
		[{_,Status,_}] ->
			binary_to_list(Status)
	end,
	{reply, Reply, Tab};

handle_call({put, Who, Status, Time}, _From, Tab) ->
	CTime = calendar:datetime_to_gregorian_seconds({date(), time()}),
	Reply = ets:insert(Tab,{list_to_binary(Who),list_to_binary(Status),CTime+Time}),
	{reply, Reply, Tab};

handle_call({get_by_date, FromDate, ToDate}, _From, Tab) ->
	FromSec = calendar:datetime_to_gregorian_seconds(FromDate),
	ToSec = calendar:datetime_to_gregorian_seconds(ToDate),
	Reply = get_by_date(ets:first(Tab), FromSec, ToSec, []),
	{reply, Reply, Tab};

handle_call(stop, _From, Tab) ->
	{stop, normal, stopped, Tab}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_test() -> [
	?_assert(start({derop_interval,10}) =:= ok)].

custodian_test() -> [
	?_assert(custodian() =:= clear_table_done)].

put_emo_test() -> [
	?_assert(put_emo({"Homer",":-)"},20) =:= true),
	?_assert(put_emo({"Moe",":-|"},20) =:= true)].

get_emo_test() -> [
	?_assert(get_emo("Homer") =:= ":-)"),
	?_assert(get_emo("Moe") =:= ":-|")].

get_by_date_test() -> [
	?_assert(get_by_date({date(), time()}, calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds({date(), time()}) + 30)) =:= {ok, [{"Homer",":-)"},{"Moe",":-|"}]})].

stop_test() -> [
	?_assert(stop() =:= true)].

-endif.
