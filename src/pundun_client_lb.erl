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

%% DB API exports
-export([get_all_nodes/0,
	 get_node/0,
	 get_node/1
	]).


get_all_nodes() ->
    gb_conf:get_param("pundun_client.yaml", pundun_nodes).


get_node(Strategy) ->
    %% Ignore Strategy for now
    get_node().

get_node() ->
    PundunNodes = gb_conf:get_param("pundun_client.yaml", pundun_nodes),
    get_pundun_node(PundunNodes).

get_pundun_node([]) ->
    undefined;
get_pundun_node(undefined) ->
    undefined;
get_pundun_node([Node | Rest]) when is_atom(Node) ->
    case net_adm:ping(Node) of
	pong ->
	    Node;
	pang ->
	    get_pundun_node(Rest)
    end;
get_pundun_node([Node | Rest]) when is_list(Node) ->
    AtomNode = list_to_atom(Node),
    case net_adm:ping(AtomNode) of
	pong ->
	    AtomNode;
	pang ->
	    get_pundun_node(Rest)
    end.

