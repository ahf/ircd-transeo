%%%
%%% Copyright (c) 2013 Alexander Færøy.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%%   list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%%   this list of conditions and the following disclaimer in the documentation
%%%   and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Transeo's IRC SID mapping server
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_sid_mapping).
-behaviour(gen_server).

%% API.
-export([start_link/1, from_source/2]).

%% Our `gen_server' callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types.
-type sid_source() :: transeo_types:sid_source().

-record(state, {
        generator :: fun(),
        map_table :: ets:tid()
    }).

-define(SERVER, ?MODULE).

%% @doc Start sid mapping server
-spec start_link(SidGenerator :: fun()) -> {ok, pid()} | ignore | {error, term()}.
start_link(SidGeneratorFun) ->
    gen_server:start_link(?MODULE, [SidGeneratorFun], []).

%% @doc Get Sid from another Sid source.
-spec from_source(Pid :: pid(), SidSource :: sid_source()) -> binary().
from_source(Pid, SidSource) ->
    gen_server:call(Pid, {from_source, SidSource}).

%% @private
-spec init([term()]) -> {ok, State :: term()} | {ok, State :: term(), Timeout :: non_neg_integer()}.
init([SidGeneratorFun]) ->
    {ok, #state {
            generator = SidGeneratorFun,
            map_table = ets:new(transeo_sid_mapping, [ordered_set, private])
        }}.

%% @private
-spec handle_call(Request :: term(), From :: pid(), State :: term()) -> {reply, Reply :: term(), NewState :: term()}.
handle_call({from_source, SidSource}, _From, #state { generator = Generator, map_table = MapTable } = State) ->
    Reply = get_or_create_unique_sid(MapTable, Generator, SidSource),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
-spec handle_cast(Request :: term(), State :: term()) -> {noreply, NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Request :: term(), State :: term()) -> {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.
handle_info(_info, state) ->
    {noreply, state}.

%% @private
-spec terminate(Reason :: term(), State :: term()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(OldVersion :: term(), State :: term(), Extra :: term()) -> {ok, NewState :: term()}.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% @private
-spec get_or_create_unique_sid(MapTable :: ets:tid(), Generator :: fun(), SourceSid :: sid_source()) -> binary().
get_or_create_unique_sid(MapTable, Generator, SidSource) ->
    case ets:match_object(MapTable, {SidSource, '_'}) of
        [{_, Sid}] ->
            Sid;

        [] ->
            create_unique_sid(MapTable, Generator, SidSource)
    end.

%% @private
-spec create_unique_sid(MapTable :: ets:tid(), Generator :: fun(), SourceSid :: sid_source()) -> binary().
create_unique_sid(MapTable, Generator, SidSource) ->
    UniqueSid = Generator(),
    case ets:match_object(MapTable, {'_', UniqueSid}) of
        [{_, UniqueSid}] ->
            create_unique_sid(MapTable, Generator, SidSource);

        [] ->
            lager:info("Creating new server map: ~p -> ~s", [SidSource, UniqueSid]),
            ets:insert(MapTable, {SidSource, UniqueSid}),
            UniqueSid
    end.
