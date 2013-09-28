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
-export([start_link/3, dispatch/2, broadcast/2, stop/1]).

%% Our `gen_fsm' states.
-export([pass/2, capab/2, server/2, svinfo/2, burst/2, normal/2]).

%% Our `gen_fsm' callbacks.
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% Types.
-type message() :: transeo_types:message().
-type ratbox_capability() :: transeo_types:ratbox_capability().

-record(state, {
        %% Same as the name in the listener.
        name :: string(),

        %% Same as the options in the listener.
        options :: proplists:proplist(),

        %% Our listener.
        listener :: pid(),

        %% Capabilities.
        capabilities = [] :: [ratbox_capability()],

        %% Sid map.
        sid_map :: pid()
    }).

-define(SERVER, ?MODULE).

-include("transeo.hrl").

%% @doc Start Ratbox Protocol FSM.
-spec start_link(ListenerPid :: pid(), Name :: string(), Options :: proplists:proplist()) -> {ok, pid()} | ignore | {error, term()}.
start_link(ListenerPid, Name, Options) ->
    gen_fsm:start_link(?SERVER, [ListenerPid, Name, Options], []).

%% @doc Dispatch a given message to the FSM.
-spec dispatch(Pid :: pid(), Message :: message()) -> ok.
dispatch(Pid, Message) ->
    gen_fsm:send_event(Pid, {dispatch, Message}).

%% @doc Used by the router to sent broadcast from other servers to our FSM.
-spec broadcast(Pid :: pid(), Message :: message()) -> ok.
broadcast(Pid, Message) ->
    gen_fsm:send_all_state_event(Pid, {broadcast, Message}).

%% @doc Stop our FSM.
-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, {stop}).

%% @private
%% This state represents the initial state whereby the connecting server have
%% yet to authorize itself.
%% The expected IRC message is: "PASS".
-spec pass({dispatch, Message :: message()}, State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()}.
pass({dispatch, #message { command = <<"PASS">>, parameters = [Password, <<"TS">>, <<"6">>, _Sid] }}, #state { options = Options } = State) ->
    case authenticate(Options, binary_to_list(Password)) of
        true ->
            log(State, info, "Succesfully authenticated"),
            send(State, transeo_ratbox_messages:pass(password(State), sid(State))),
            {next_state, capab, State};

        false ->
            log(State, warning, "Authentication failed"),
            {stop, normal, State}
    end;

%% We only allow the PASS message when we are in pass state.
pass({dispatch, _Message}, State) ->
    {stop, normal, State}.

%% @private
%% Negotiate capabilities.
%% The expected IRC message is: "CAPAB".
-spec capab({dispatch, Message :: message()}, State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()}.
capab({dispatch, #message { command = <<"CAPAB">>, parameters = [RawCapabilities] }}, State) ->
    Capabilities = transeo_ratbox_utilities:decode_capabilities(RawCapabilities),
    log(State, info, "Capabilities: ~p", [Capabilities]),
    send(State, transeo_ratbox_messages:capab(Capabilities)),
    {next_state, server, State#state { capabilities = Capabilities }};

capab({dispatch, _Message}, State) ->
    {stop, normal, State}.

%% @private
%% Remote server information.
%% The expected IRC message is: "SERVER".
-spec server({dispatch, Message :: message()}, State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()}.
server({dispatch, #message { command = <<"SERVER">> }}, State) ->
    send(State, transeo_ratbox_messages:server(transeo_config:name(), 1, transeo_config:description())),
    {next_state, svinfo, State};

server({dispatch, _Message}, State) ->
    {stop, normal, State}.

%% @private
%% Remote server TS information.
%% The expected IRC message is: "SVINFO".
-spec svinfo({dispatch, Message :: message()}, State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()}.
svinfo({dispatch, #message { command = <<"SVINFO">> }}, State) ->
    send(State, transeo_ratbox_messages:svinfo(transeo_utilities:timestamp())),
    {next_state, burst, State};

svinfo({dispatch, #message { command = <<"ERROR">>, parameters = [Error]}}, #state { listener = ListenerPid } = State) ->
    log(State, error, "Error: ~s", [Error]),
    transeo_listener:disconnect(ListenerPid),
    {stop, normal, State};

svinfo({dispatch, _Message}, State) ->
    {stop, normal, State}.

%% @private
%% Burst state.
-spec burst({dispatch, Message :: message()}, State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()}.
burst({dispatch, #message { command = <<"UID">>, parameters = [Nickname, HopCount, Timestamp, Modes, Username, Hostname, RealHostname, Id, Realname], prefix = Sid }}, State) ->
    dispatch(#nick_message {
        nickname = Nickname,
        source = {?MODULE, Sid},
        hop_count = transeo_utilities:binary_to_integer(HopCount),
        timestamp = transeo_utilities:binary_to_integer(Timestamp),
        modes = transeo_modes:parse(Modes),
        username = Username,
        hostname = Hostname,
        real_hostname = RealHostname,
        id = Id,
        realname = Realname
    }),
    {next_state, burst, State};

burst({dispatch, #message { command = <<"SJOIN">> }}, State) ->
    {next_state, burst, State};

burst({dispatch, #message { command = <<"PING">>, parameters = [Sid] }}, State) ->
    log(State, info, "End of Burst"),
    send(State, transeo_ratbox_messages:pong(Sid)),
    dispatch(#eob_message {}),
    {next_state, normal, State};

burst({dispatch, _Message}, State) ->
    {stop, normal, State}.

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
    ok = transeo_router:register_peer({?MODULE, self()}),
    {ok, SidMap} = transeo_sid_mapping:start_link(fun transeo_ratbox_utilities:create_random_sid/0),
    {ok, pass, #state {
            name = Name,
            listener = ListenerPid,
            options = Options,
            sid_map = SidMap
        }}.

%% @private
-spec handle_event(Event :: term(), StateName :: atom(), State :: term()) -> {next_state, StateName :: atom(), State :: term()} | {stop, Reason :: term(), State :: term()}.
handle_event({broadcast, Message}, StateName, State) ->
    log(State, info, "Broadcast: ~p", [Message]),
    {next_state, StateName, State};

handle_event({stop}, _StateName, State) ->
    {stop, normal, State};

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
    ok = transeo_router:unregister_peer({?MODULE, self()}),
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
    lager:log(LogLevel, [{ratbox_fsm, Name}], "~s: " ++ Format, [Name | Arguments]).

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
-spec sid(State :: term()) -> string().
sid(#state { options = Options }) ->
    proplists:get_value(sid, Options).

%% @private
-spec send(State :: term(), Message :: iolist()) -> ok.
send(#state { listener = Listener }, Message) ->
    transeo_listener:send(Listener, Message).

%% @private
-spec dispatch(Message :: message()) -> ok.
dispatch(Message) ->
    transeo_router:dispatch({?MODULE, self()}, Message).
