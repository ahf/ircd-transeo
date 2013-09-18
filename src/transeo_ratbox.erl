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
%%% @doc Ratbox Protocol FSM.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_ratbox).
-behaviour(gen_fsm).

%% API.
-export([start_link/1, dispatch/2]).

%% Our `gen_fsm' states.
-export([pass/2, capab/2]).

%% Our `gen_fsm' callbacks.
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% Types.
-type message() :: transeo_types:message().

-record(state, {
        listener :: pid()
    }).

-define(SERVER, ?MODULE).

-include("transeo.hrl").

%% @doc Start Ratbox Protocol FSM.
-spec start_link(ListenerPid :: pid()) -> {ok, pid()} | ignore | {error, term()}.
start_link(ListenerPid) ->
    gen_fsm:start_link(?SERVER, [ListenerPid], []).

%% @doc Dispatch a given message to the FSM.
-spec dispatch(Pid :: pid(), Message :: message()) -> ok.
dispatch(Pid, Message) ->
    gen_fsm:send_event(Pid, {dispatch, Message}).

%% @private
%% This state represents the initial state whereby the connecting server have
%% yet to authorize itself.
%% The expected IRC message is: "PASS".
-spec pass({dispatch, Message :: message()}, State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()}.
pass({dispatch, #message { command = <<"PASS">>, parameters = [Password, <<"TS">>, <<"6">>, _Sid] }}, #state { listener = Listener } = State) ->
    case transeo_listener:authenticate(Listener, binary_to_list(Password)) of
        true ->
            lager:info("Succesfully authenticated"),
            {next_state, capab, State};

        false ->
            lager:warning("Unable to authenticate remote server"),
            {stop, normal, State}
    end;

%% We only allow the PASS message when we are in pass state.
pass({dispatch, _Message}, State) ->
    {stop, normal, State}.

%% @private
%% Negotiate capabilities.
%% The expected IRC message is: "CAPAB".
-spec capab({dispatch, Message :: message()}, State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()}.
capab({dispatch, #message { command = <<"CAPAB">>, parameters = [_Capabilities] }}, State) ->
    {next_state, capab, State};

capab({dispatch, _Message}, State) ->
    {stop, normal, State}.

%% @private
-spec init([term()]) -> {ok, StateName :: atom(), State :: term()}.
init([ListenerPid]) ->
    {ok, pass, #state {
            listener = ListenerPid
        }}.

%% @private
-spec handle_event(Event :: term(), StateName :: atom(), State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()}.
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @private
-spec handle_sync_event(Event :: term(), From :: pid(), StateName :: atom(), State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {reply, Reply :: term(), StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()} | {stop, Reason :: term(), Reply :: term(), State :: term()}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% @private
-spec handle_info(Info :: term(), StateName :: atom(), State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%% @private
-spec terminate(Reason :: term(), StateName :: atom(), State :: term()) -> ok.
terminate(_Reason, _StateName, _State) ->
    ok.

%% @private
-spec code_change(OldVersion :: term(), StateName :: atom(), State :: term(), Extra :: term()) -> {ok, StateName :: atom(), State :: term()}.
code_change(_OldVersion, StateName, State, _Extra) ->
    {ok, StateName, State}.
