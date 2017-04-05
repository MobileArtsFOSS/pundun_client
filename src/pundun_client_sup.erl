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
%% @doc pundun_client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pundun_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Child) ->
    supervisor:start_child(?MODULE, [Child]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    PundunWorker = get_pundun_worker_spec(),
    {ok, {#{strategy => one_for_one, 
	   intensity => 0, 
	   period => 1}, [PundunWorker]}}.

%%====================================================================
%% Internal functions
%%====================================================================
get_pundun_worker_spec() ->
    #{id => pundun_client_worker,
      start => {pundun_client_worker,
		start_link,
		[]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [pundun_client_worker]}.
