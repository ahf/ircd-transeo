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
%%% @doc The Transeo Application.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_app).
-behaviour(application).

%% API.
-export([start/0, start/2, stop/1]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(transeo).

-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()}.
start(_, _) ->
    case transeo_sup:start_link() of
        {ok, _} = Result ->
            start_listeners(transeo_config:listeners()),
            Result;

        {error, _} = Error ->
            Error
    end.

-spec stop([]) -> ok.
stop(_State) ->
    ok.

%% @doc Start TCP Listeners.
-spec start_listeners([]) -> ok.
start_listeners([{Name, Options} | Rest]) ->
    Port = proplists:get_value(accept_port, Options),
    Protocol = proplists:get_value(protocol, Options),
    lager:info("Starting Listener for IRC server: ~s:~b (Protocol: ~p)", [Name, Port, Protocol]),
    {ok, _} = ranch:start_listener(Name, 100, ranch_tcp, [{port, Port}], transeo_listener, [Name, Options]),
    start_listeners(Rest);

start_listeners([]) ->
    ok.
