-module(riak_ensemble_ng_demo).
%% I just copy the functions from https://github.com/marksteele/riak_ensemble_demo
%% which is according to the riak_ensemble, and the Erlang/OTP 17.0
%% I update it with riak_ensemble_ng, rebar3, Erlang/OTP 19

%% API
-export([
		 cluster_status/0,
		 read/1,
		 read_object/1,
		 write/2,
		 cas/3,
		 write_once/2,
		 create_ensemble/0,
		 join_cluster/1
		]).


%%%===================================================================
%%% API
%%%===================================================================
-record(obj,{epoch,seq,key,value}).

read(Key) ->
	case read_object(Key) of
		{ok,#obj{value=notfound}} ->
			{error, not_found};
		{ok,Obj} ->
			{ok, Obj#obj.value};
		Error ->
			Error
	end.

read_object(Key) ->
	case riak_ensemble_client:kget(node(),root,Key,10000) of
		{ok,Obj} ->
			{ok, Obj};
		Error ->
			{error, Error}
	end.

write(Key, Value) ->
	case riak_ensemble_client:kover(node(),root,Key,Value,10000) of
		{ok, _} ->
			ok;
		Err ->
			{error, Err}
	end.

cas(Key, OldObj, New) ->
	case riak_ensemble_client:kupdate(node(),root,Key,OldObj,New,10000) of
		{ok,_} ->
			ok;
		Error ->
			{error, Error}
	end.

write_once(Key,Value) ->
	case riak_ensemble_client:kput_once(node(),root,Key,Value,10000) of
		{ok, _} ->
			ok;
		Error ->
			{error, Error}
	end.

cluster_status() ->
	case riak_ensemble_manager:enabled() of
		false ->
			{error, not_enabled};
		true ->
			Nodes = lists:sort(riak_ensemble_manager:cluster()),
			io:format("Nodes in cluster: ~p~n",[Nodes]),
			LeaderNode = node(riak_ensemble_manager:get_leader_pid(root)),
			io:format("Leader: ~p~n",[LeaderNode])
	end.

create_ensemble() ->
	%% create a abc ensemble for test
	riak_ensemble_manager:create_ensemble(abc, undefined, [{abc, node()} | [{abc, N} || N <- nodes()]],
										  riak_ensemble_basic_backend, []).
join_cluster(Node) ->
	case node() of
		Node ->
			lager:error("can not join local node, use a new node name", []);
		_ ->
			case riak_ensemble_manager:join(Node, node()) of
				ok ->
					wait_stable(),
					riak_ensemble_peer:update_members(
					  riak_ensemble_manager:get_leader_pid(root),
					  [{add,{root,node()}}],
					  5000);
				_ ->
					ok
			end
	end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------




%%%===================================================================
%%% Internal functions
%%%===================================================================
wait_stable() ->
	case check_stable() of
		true ->
			ok;
		false ->
			wait_stable()
	end.

check_stable() ->
	case riak_ensemble_manager:check_quorum(root, 1000) of
		true ->
			case riak_ensemble_peer:stable_views(root, 1000) of
				{ok, true} ->
					true;
				_ ->
					false
			end;
		false ->
			false
	end.
