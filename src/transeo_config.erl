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
%%% @doc Configuration Server
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_config).
-behaviour(gen_server).

%% API.
-export([start_link/0, name/0, description/0, listeners/0, mappings/0]).

%% Our `gen_server' callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types.
-record(state, {}).

-define(SERVER, ?MODULE).

%% @doc Start configuration server.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Our name.
-spec name() -> string().
name() ->
    call(name).

%% @doc Our description.
-spec description() -> string().
description() ->
    call(description).

%% @doc Our SID mappings.
-spec mappings() -> [atom()].
mappings() ->
    call(mappings).

%% @doc Get listener specifications.
-spec listeners() -> [term()].
listeners() ->
    call(listeners).

%% @private
-spec init([]) -> {ok, State :: term()}.
init([]) ->
    {ok, #state {} }.

%% @private
-spec handle_call(Request :: term(), From :: pid(), State :: term()) -> {noreply, NewState :: term()} | {reply, Reply :: term(), NewState :: term()}.
handle_call({value, Key}, _From, State) ->
    {reply, value(Key), State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
-spec handle_cast(Message :: term(), State :: term()) -> {noreply, NewState :: term()}.
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info :: term(), State :: term()) -> {noreply, NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason :: term(), State :: term()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(OldVersion :: term(), State :: term(), Extra :: term()) -> {ok, NewState :: term()}.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% @private
-spec value(Key :: term()) -> not_found | term().
value(Key) ->
    case application:get_env(transeo, Key) of
        undefined ->
            not_found;

        {ok, Value} ->
            Value
    end.

%% @private
-spec call(Key :: term()) -> term().
call(Key) ->
    case gen_server:call(?SERVER, {value, Key}) of
        not_found ->
            exit({missing_config_value, Key});

        Value ->
            Value
    end.
