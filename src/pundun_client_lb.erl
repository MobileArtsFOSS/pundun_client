%%%===================================================================
%% @author Hazim Sultan
%% @copyright 2017 Mobile Arts AB
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%-------------------------------------------------------------------
%% @doc pundun_client worker.
%% @end
%%%-------------------------------------------------------------------
-module(pundun_client_lb).

-behaviour(gen_server).

-include_lib("gb_log/include/gb_log.hrl").

%%-compile(export_all).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% DB API exports
-export([active_nodes/0,
	 active_local_nodes/0,
	 bad_nodes/0,
	 get_node/0,
	 get_node/1,
         check_nodes/1,
         check_node/1,
         load_cfg/0
	]).

-record(state, {born}).

%%%===================================================================
%%% API
%%%===================================================================

active_nodes() ->
    case get_from_pundun_ctable(pundun_info) of
	{ok, #{p_active_nodes := An}} ->
	    An;
	Else ->
	    Else
    end.

active_local_nodes() ->
    case get_from_pundun_ctable(pundun_info) of
	{ok, #{p_local_nodes := Ln}} ->
	    Ln;
	Else ->
	    Else
    end.

bad_nodes() ->
    case get_from_pundun_ctable(pundun_info) of
	{ok, #{p_bad_nodes := Bn}} ->
	    Bn;
	Else ->
	    Else
    end.

get_node(Strategy) when Strategy == local ->
    case active_local_nodes() of
	[] ->
	    ?debug("Warning: no active pundun nodes on local node", []),
	    get_node(first);
	[FirstLocal | _] ->
	    FirstLocal
    end;
get_node(first) ->
    case active_nodes() of
	[First | _] ->
	    First;
	[] ->
	    ?debug("Warning: no active pundun nodes", []),
	    {error, no_active_pundun_nodes}
    end;
get_node(_Unknown) ->
    ?debug("warning: unkown strategy = ~p, use local", [_Unknown]),
    get_node(local).

get_node() ->
    get_node(local).

load_cfg() ->
    gen_server:cast(?SERVER, load_cfg).

check_nodes({R, N}) ->
    check_node({R, N}).

check_node({Reason, Node}) ->
    gen_server:cast(?SERVER, {check_node, {Reason, Node}}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    CfgNodes = get_config_nodes(),
    BadNodes = get_bad_nodes(CfgNodes),
    ActiveNodes = CfgNodes -- BadNodes,
    LocalNodes  = get_local_nodes(ActiveNodes),
    PundunInfo = #{'#mt' => pundun_info,
		   p_nodes => CfgNodes,
		   p_active_nodes => ActiveNodes,
		   p_local_nodes => LocalNodes,
		   p_bad_nodes => BadNodes},
    ?debug("Init pundun_info = ~p", [PundunInfo]),
    ets:insert(pundun_ctable, {pundun_info, PundunInfo}),
    {ok, #state{born = os:timestamp()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({check_node, {Reason, Node}}, State) ->
    case is_active_node(Node) of
	true ->
            ?debug("Check node = ~p due to reason = ~p, but node is responding", [Node, Reason]),
	    ok;
	_ ->
            ?debug("Check node = ~p due to reason = ~p, node is not responding, remove from active nodes", [Node, Reason]),
	    remove_from_active_nodes(Node)
    end,
    {noreply, State};
handle_cast(load_cfg, State) ->
    load_cfg_local(State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_cfg_local(_State) ->
    CfgNodes = get_config_nodes(),
    BadNodes = get_bad_nodes(CfgNodes),
    ActiveNodes = CfgNodes -- BadNodes,
    LocalNodes  = get_local_nodes(ActiveNodes),
    PundunInfo = #{'#mt' => pundun_info,
		   p_nodes => CfgNodes,
		   p_active_nodes => ActiveNodes,
		   p_local_nodes => LocalNodes,
		   p_bad_nodes => BadNodes},
    ?debug("Load cfg: pundun_info = ~p", [PundunInfo]),
    ets:insert(pundun_ctable, {pundun_info, PundunInfo}),
    ok.

get_bad_nodes(ActiveNodes) ->
    get_bad_nodes(ActiveNodes, []).

get_bad_nodes([], Acc) ->
    lists:reverse(Acc);
get_bad_nodes([N | Rest], Acc) ->
    case is_active_node(N) of
	true ->
	    get_bad_nodes(Rest, Acc);
	false ->
	    get_bad_nodes(Rest, [N | Acc])
    end.

is_active_node(N) ->
    case net_adm:ping(N) of
	pong ->
	    true;
	_ ->
	    false
    end.

get_local_nodes(ActiveNodes) ->
    get_local_nodes(ActiveNodes, hostname(node()), []).

get_local_nodes([], _, Acc) ->
    lists:reverse(Acc);
get_local_nodes([N | Rest], Ln, Acc) ->
    case hostname(N) == Ln of
	true ->
	    get_local_nodes(Rest, Ln, [N | Acc]);
	false ->
	    get_local_nodes(Rest, Ln, Acc)
    end.

hostname(NodeName) when is_atom(NodeName) ->
    hostname(atom_to_list(NodeName));
hostname(NodeName) when is_list(NodeName) ->
    case lists:splitwith(fun(E) -> E /= $@ end, NodeName) of
	{_, [$@ | HostN]} ->
	    HostN;
	_ ->
	    {error, {not_proper_node_name, NodeName}}
    end.

remove_from_active_nodes(Node) ->
    case ets:lookup(pundun_ctable, pundun_info) of
	[{pundun_info, Pi}] ->
	    Pi1 = remove_from_active_nodes(Node, Pi),
	    ets:insert(pundun_ctable, Pi1);
	Else ->
	    ?debug("Bad configuration, pundun_info not present in pundun_ctable err: ~p", [Else]),
	    throw({error, {bad_conf, pundun_ctable, Else}})
    end.

remove_from_active_nodes(Node, #{p_active_nodes := An, p_local_nodes := Ln, p_bad_nodes := Bn} = Pi) ->
    An1 = lists:delete(Node, An),
    Ln1 = lists:delete(Node, Ln),
    Bn1 = add_to_list(Node, Bn),
    Pi1 = Pi#{p_active_nodes := An1, p_local_nodes := Ln1, p_bad_nodes := Bn1},
    Pi1.

add_to_list(Node, L) ->
    case lists:member(Node, L) of
	true ->
	    ?debug("Node = ~p is already member of L = ~p", [Node, L]),
	    ok;
	false ->
	    [Node | L]
    end.

get_from_pundun_ctable(El) ->
    case ets:lookup(pundun_ctable, El) of
	[{El, Val}] ->
	    {ok, Val};
	Else ->
	    {error, {pundun_ctable, El, Else}}
    end.

get_config_nodes() ->
    [list_to_atom(X) || X <- gb_conf:get_param("pundun_client.yaml", pundun_nodes)].
