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
%% @doc pundun_client public API
%% @end
%%%-------------------------------------------------------------------

-module(pundun_client_app).

-behaviour(application).
%% Application callbacks
-export([start/2, stop/1]).

%% Hack to make this supervisor as well
%% This will then just load the modules and make sure a
%% pundun_client app exists
-behaviour(supervisor).
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

init([]) ->
    ChildrenSups = get_child_supervisors(),
    %% maybe do something here later.
    {ok, {{one_for_one,10,1},ChildrenSups}}.

%%====================================================================
%% Internal functions
%%====================================================================
get_child_supervisors() ->
    GenSup = {pundun_client_sup, {pundun_client_sup, start_link, []},
	      permanent, 5000, supervisor, [pundun_client_sup]},
    [GenSup].
