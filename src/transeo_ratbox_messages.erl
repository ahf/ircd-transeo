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
%%% @doc Ratbox Protocol Messages.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_ratbox_messages).

%% API.
-export([pass/2, capab/1, server/3, svinfo/1, ping/1, pong/1]).

%% Types.
-type ratbox_capability() :: transeo_types:ratbox_capability().

-define(TS_CURRENT, 6).
-define(TS_MIN, 6).

%% @doc
%% Create PASS message.
-spec pass(Password :: string(), Sid :: string()) -> iolist().
pass(Password, Sid) ->
    io_lib:format("PASS ~s TS 6 :~s", [Password, Sid]).

%% @doc
%% Create CAPAB message.
-spec capab(Capabilities :: [ratbox_capability()]) -> iolist().
capab(Capabilities) ->
    [<<"CAPAB :">>, transeo_ratbox_utilities:encode_capabilities(Capabilities)].

%% @doc
%% Create SERVER message.
-spec server(Hostname :: string(), HopCount :: integer(), Description :: string()) -> iolist().
server(Hostname, HopCount, Description) ->
    io_lib:format("SERVER ~s ~b :~s", [Hostname, HopCount, Description]).

%% @doc
%% Create SVINFO message.
-spec svinfo(Timestamp :: non_neg_integer()) -> iolist().
svinfo(Timestamp) ->
    io_lib:format("SVINFO ~b ~b 0 :~b", [?TS_CURRENT, ?TS_MIN, Timestamp]).

%% @doc
%% Create PING message.
-spec ping(Data :: string()) -> iolist().
ping(Data) ->
    io_lib:format("PING :~s", [Data]).

%% @doc
%% Create PONG message.
-spec pong(Data :: string()) -> iolist().
pong(Data) ->
    io_lib:format("PONG :~s", [Data]).
