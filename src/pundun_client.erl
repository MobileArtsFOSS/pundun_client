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
%% @doc pundun_client API.
%% @end
%%%-------------------------------------------------------------------

-module(pundun_client).

%% DB API's
-export([read/2,
	 write/3,
	 create_table/3,
	 open_table/1,
	 close_table/1,
	 table_info/1,
	 table_info/2,
	 update/3,
	 delete/2,
	 read_range/4,
	 read_range_n/3,
	 batch_write/3,
	 first/1,
	 last/1,
	 seek/2,
	 next/1,
	 prev/1
	]).

%% ===================================================================
%% Pundun Database API's
%% ===================================================================

read(TabName, Key) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, read, [TabName, Key]).

write(TabName, Key, Columns) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, write, [TabName, Key, Columns]).

create_table(TabName, KeyDef, Options) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, create_table, [TabName, KeyDef, Options]).

open_table(TabName) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, open_table, [TabName]).

close_table(TabName) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, close_table, [TabName]).

table_info(TabName) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, table_info, [TabName]).

table_info(TabName, Attributes) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, table_info, [TabName, Attributes]).

update(TabName, Key, Op) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, update, [TabName, Key, Op]).

delete(TabName, Key) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, delete, [TabName, Key]).

read_range(TabName, StartKey, EndKey, Chunk) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, read_range,[TabName, StartKey, EndKey, Chunk]).

read_range_n(TabName, StartKey, N) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, read_range_n, [TabName, StartKey, N]).

batch_write(TabName, DeleteKeys, WriteKvps) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, batch_write, [TabName, DeleteKeys, WriteKvps]).

first(TabName) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, first, [TabName]).

last(TabName) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, last, [TabName]).

seek(TabName, Key) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, seek, [TabName, Key]).

next(Ref) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, next, [Ref]).

prev(Ref) ->
    PundunNode = pundun_client_lb:get_node(),
    exec_pundun_command(PundunNode, prev, [Ref]).


exec_pundun_command(InvalidNode, _, _) when InvalidNode == undefined;
					    InvalidNode == [] ->
    {error, db_not_available};
exec_pundun_command(PNode, Command, Arg) ->
    case rpc:call(PNode, enterdb, Command, Arg, 5000) of
	{badrpc, _} ->
	    {error, db_not_available};
	Resp ->
	    Resp
    end.
