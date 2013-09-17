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
%%% @doc Transeo's IRC Protocol Listener.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_listener).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API.
-export([]).

%% Our `gen_server' callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Our `ranch_protocol' callbacks.
-export([start_link/4]).

%% Types.
-type message() :: transeo_types:message().

-record(state, {
        %% The name of our listener.
        name :: string(),

        %% Options from `sys.config'.
        options :: proplists:proplist(),

        %% Continuation of our data chunk.
        continuation = <<>> :: binary(),

        %% Are we an inbound connection?
        inbound :: boolean(),

        %% Ranch's Listener Pid.
        listener :: pid() | undefined,

        %% Socket.
        socket :: inet:socket() | undefined
    }).

-define(SERVER, ?MODULE).

-include("transeo.hrl").

%% @private
-spec init([term()]) -> {ok, State :: term()} | {ok, State :: term(), Timeout :: non_neg_integer()}.
init([ListenerPid, Socket, [Name, Options]]) ->
    %% Note: See handle_info(timeout, ...) for final initialization.
    {ok, #state {
            name = Name,
            options = Options,
            inbound = true,
            listener = ListenerPid,
            socket = Socket
        }, 0}.

%% @private
-spec handle_call(Request :: term(), From :: pid(), State :: term()) -> {reply, Reply :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
-spec handle_cast(Request :: term(), State :: term()) -> {noreply, NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Request :: term(), State :: term()) -> {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.
handle_info(timeout, #state { listener = ListenerPid, socket = Socket } = State) ->
    ok = ranch:accept_ack(ListenerPid),
    ack_socket(Socket),
    {noreply, State};

handle_info({tcp, Socket, Packet}, #state { socket = Socket, options = Options, continuation = Continuation } = State) ->
    Data = <<Continuation/binary, Packet/binary>>,
    case parse(Data, Options) of
        {ok, Messages, NewContinuation} ->
            lists:foreach(fun (Message) ->
                        log(State, info, "P: ~s, C: ~s, A: ~p", [Message#message.prefix, Message#message.command, lists:map(fun binary_to_list/1, Message#message.parameters)])
                end, Messages),
            {noreply, State#state { continuation = NewContinuation }};

        {error, Reason} = Error ->
            log(State, error, "Parse Error: ~p", [Reason]),
            {stop, Error, State}
    end;

handle_info({tcp_closed, Socket}, #state { socket = Socket } = State) ->
    {stop, normal, State};

handle_info({tcp_error, Socket, Reason}, #state { socket = Socket } = State) ->
    {stop, Reason, State};

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
%% This is the `ranch_protocol' callback to start a listener.
-spec start_link(ListenerPid :: pid(), Socket :: inet:socket(), Transport :: term(), Options :: term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(ListenerPid, Socket, _Transport, Options) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Options], []).

%% @private
-spec ack_socket(Socket :: inet:socket()) -> ok.
ack_socket(Socket) ->
    inet:setopts(Socket, [{active, once}]).

%% @private
-spec log(State :: term(), LogLevel :: atom(), Format :: string()) -> ok.
log(State, LogLevel, Format) ->
    log(State, LogLevel, Format, []).

%% @private
-spec log(State :: term(), LogLevel :: atom(), Format :: string(), Arguments :: [term()]) -> ok.
log(#state { name = Name }, LogLevel, Format, Arguments) ->
    lager:log(LogLevel, [{listener, Name}], "~s: " ++ Format, [Name | Arguments]).

%% @private
-spec parse(Data :: binary(), Options :: proplists:proplist()) -> {ok, [message()], Chunk :: binary()} | {error, term()}.
parse(Data, Options) ->
    WireProtocolModule = proplists:get_value(wire_protocol, Options),
    apply(WireProtocolModule, parse, [Data]).
