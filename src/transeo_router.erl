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
%%% @doc Message Router.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_router).
-behaviour(gen_server).

%% API.
-export([start_link/0, register_peer/1, unregister_peer/1, dispatch/2]).

%% Our `gen_server' callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types.
-type message() :: transeo_types:message().
-type peer() :: transeo_types:peer().

-record(state, {
        peers = [] :: [peer()]
    }).

-define(SERVER, ?MODULE).

-include("transeo.hrl").

%% @doc Start router.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Dispatch message to router.
-spec dispatch(Self :: peer(), Message :: message()) -> ok.
dispatch(Self, Message) ->
    gen_server:cast(?SERVER, {dispatch, Self, Message}).

%% @doc Register peer to router.
-spec register_peer(Peer :: peer()) -> ok.
register_peer(Peer) ->
    gen_server:cast(?SERVER, {register_peer, Peer}).

%% @doc Unregister peer to router.
-spec unregister_peer(Peer :: peer()) -> ok.
unregister_peer(Peer) ->
    gen_server:cast(?SERVER, {unregister_peer, Peer}).

%% @private
-spec init([]) -> {ok, State :: term()} | {ok, State :: term(), Timeout :: non_neg_integer()}.
init([]) ->
    {ok, #state {}}.

%% @private
-spec handle_call(Request :: term(), From :: pid(), State :: term()) -> {reply, Reply :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
-spec handle_cast(Request :: term(), State :: term()) -> {noreply, NewState :: term()}.
handle_cast({dispatch, Peer, Message}, State) ->
    lager:notice("Router: ~p from ~p", [Message, Peer]),
    {noreply, State};

handle_cast({register_peer, Peer}, #state { peers = Peers } = State) ->
    lager:info("Router: Registering peer: ~p", [Peer]),
    {noreply, State#state { peers = [Peer | Peers] }};

handle_cast({unregister_peer, Peer}, #state { peers = Peers } = State) ->
    lager:info("Router: Unregistering peer: ~p", [Peer]),
    {noreply, State#state { peers = [Peers] -- [Peer] }};

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
