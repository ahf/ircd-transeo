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
%%% @doc IRCnet Ircd Protocol FSM.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_ircd).
-behaviour(gen_fsm).

%% API.
-export([start_link/3, dispatch/2]).

%% Our `gen_fsm' states.
-export([normal/2]).

%% Our `gen_fsm' callbacks.
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% Types.
-type message() :: transeo_types:message().

-record(state, {
        %% Same as the name in the listener.
        name :: string(),

        %% Same as the options in the listener.
        options :: proplists:proplist(),

        %% Our listener.
        listener :: pid()
    }).

-define(SERVER, ?MODULE).

-include("transeo.hrl").

%% @doc Start IRCnet Ircd Protocol FSM.
-spec start_link(ListenerPid :: pid(), Name :: string(), Options :: proplists:proplist()) -> {ok, pid()} | ignore | {error, term()}.
start_link(ListenerPid, Name, Options) ->
    gen_fsm:start_link(?SERVER, [ListenerPid, Name, Options], []).

%% @doc Dispatch a given message to the FSM.
-spec dispatch(Pid :: pid(), Message :: message()) -> ok.
dispatch(Pid, Message) ->
    gen_fsm:send_event(Pid, {dispatch, Message}).

%% @private
%% Normal state after handshake + burst.
-spec normal({dispatch, Message :: message()}, State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()}.
normal({dispatch, #message { command = <<"PING">>, parameters = [Value] }}, State) ->
    send(State, transeo_ratbox_messages:pong(Value)),
    {next_state, normal, State};

normal({dispatch, _Message}, State) ->
    {next_state, normal, State}.

%% @private
-spec init([term()]) -> {ok, StateName :: atom(), State :: term()}.
init([ListenerPid, Name, Options]) ->
    {ok, normal, #state {
            name = Name,
            listener = ListenerPid,
            options = Options
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

%% @private
-spec log(State :: term(), LogLevel :: atom(), Format :: string()) -> ok.
log(State, LogLevel, Format) ->
    log(State, LogLevel, Format, []).

%% @private
-spec log(State :: term(), LogLevel :: atom(), Format :: string(), Arguments :: [term()]) -> ok.
log(#state { name = Name }, LogLevel, Format, Arguments) ->
    lager:log(LogLevel, [{ircd_fsm, Name}], "~s: " ++ Format, [Name | Arguments]).

%% @private
-spec authenticate(Options :: proplists:proplist(), Password :: string()) -> boolean().
authenticate(Options, Password) ->
    AcceptPassword = proplists:get_value(accept_password, Options),
    AcceptPassword =:= Password.

%% @private
-spec password(State :: term()) -> string().
password(#state { options = Options }) ->
    proplists:get_value(sent_password, Options).

%% @private
-spec send(State :: term(), Message :: iolist()) -> ok.
send(#state { listener = Listener }, Message) ->
    transeo_listener:send(Listener, Message).

