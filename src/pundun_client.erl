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
	 write/3
	]).

%% ===================================================================
%% Pundun Database API's
%% ===================================================================

read(TabName, Key) ->
    pundun_client_worker:read(TabName, Key).

write(TabName, Key, Columns) ->
    pundun_client_worker:write(TabName, Key, Columns).
