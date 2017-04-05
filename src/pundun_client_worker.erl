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

-module(pundun_client_worker).

-behaviour(gen_server).

-export([start_link/0,
	 start_link/1]).

%% DB API exports
-export([get_state/0,
	 read/2,
	 write/3]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
	 terminate/2, 
         code_change/3]).


%%====================================================================
%% @doc
%% Starts a worker that connects and manages pbpc sessions .
%% @end
%%====================================================================
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Error :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start_link(Args :: [{atom(), term()} | atom()]) ->
    {ok, Pid :: pid()} | ignore | {error, Error :: term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init(_Args) ->
    case pbpc:connect("localhost", 8887, "wla", "wla") of
	{ok, Session} ->
	    io:format("Connected to Pundun ~p", [Session]),
	    %% erlang:monitor(Session),
	    {ok, #{pbpc_sessions => [Session]}};
	Err ->
	    io:format("Couldnt Connect to Pundun: ~p", [Err]),
	    {stop, Err}
    end.

%%====================================================================
%% @private
%% @doc
%% Handling call messages
%% @end
%%====================================================================
-spec handle_call(Request :: term(),
		  From :: {pid(), Tag :: term()},
		  State :: #{}) ->
    {reply, Reply, State} |
    {reply, Reply, State, Timeout} |
    {noreply, State} |
    {noreply, State, Timeout} |
    {stop, Reason, Reply, State} |
    {stop, Reason, State}.
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({read, TabName, Key}, _From, #{pbpc_sessions := Sessions} = State) ->
    Session = hd(Sessions),
    Resp = pbpc:read(Session, TabName, Key),
    {reply, Resp, State};
handle_call({write, TabName, Key, Columns}, _From, #{pbpc_sessions := Sessions} = State) ->
    Session = hd(Sessions),
    Resp = pbpc:write(Session, TabName, Key, Columns),
    {reply, Resp, State};
handle_call(_Request, _From, State) ->
    io:format("Unhandled gen_server Request: ~p, From ~p, State: ~p",
	    [_Request, _From, State]),
    {reply, ok, State}.

%%====================================================================
%% @private
%% @doc
%% Handling cast messages
%% @end
%%====================================================================
-spec handle_cast(Msg :: term(), State :: map()) ->
    {noreply, State :: map()} |
    {noreply, State :: map(), Timeout :: integer()} |
    {stop, Reason :: term(), State :: map()}.

handle_cast(_Msg, State) ->
    io:format("Unhandled cast message received: ~p", [_Msg]),
    {noreply, State}.

%%====================================================================
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%====================================================================
-spec handle_info(Info :: term, State :: map()) ->
    {noreply, State :: map()} |
    {noreply, State :: map(), Timeout :: integer()} |
    {stop, Reason :: term(), State :: map()}.

handle_info(_Info, State) ->
    io:format("Unhandled info received: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% DB API functions
%%====================================================================

%%===================================================================
%% @doc
%% Get the current state of worker
%% @end
%%====================================================================
-spec get_state() -> State :: term().
get_state() ->
    gen_server:call(?MODULE, get_state).

%%===================================================================
%% @doc
%% Read Operation for Pundun
%% @end
%%====================================================================
read(TabName, Key) ->
    gen_server:call(?MODULE, {read, TabName, Key}).


%%===================================================================
%% @doc
%% Write Operation for Pundun
%% @end
%%====================================================================
write(TabName, Key, Columns) ->
    gen_server:call(?MODULE, {write, TabName, Key, Columns}).


