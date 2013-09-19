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
%%% @doc Message Utilities.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_message).

%% API.
-export([new/2, new/3, prefix/1, command/1, parameters/1, raw_line/1]).

%% Types.
-type message() :: transeo_types:message().
-type prefix() :: transeo_types:prefix().

-include("transeo.hrl").

%% @doc Create new message.
-spec new(Command :: binary(), Parameters :: [binary()]) -> message().
new(Command, Parameters) ->
    new(undefined, Command, Parameters).

%% @doc Create new message.
-spec new(Prefix :: binary(), Command :: binary(), Parameters :: [binary()]) -> message().
new(Prefix, Command, Parameters) ->
    #message { prefix = Prefix, command = Command, parameters = Parameters }.

%% @doc Get prefix from a given message.
-spec prefix(Message :: message()) -> prefix().
prefix(#message { prefix = Prefix }) ->
    Prefix.

%% @doc Get command from a given message.
-spec command(Message :: message()) -> binary().
command(#message { command = Command }) ->
    Command.

%% @doc Get parameters from a given message.
-spec parameters(Message :: message()) -> [binary()].
parameters(#message { parameters = Parameters }) ->
    Parameters.

%% @doc Get raw line from a given message.
-spec raw_line(Message :: message()) -> binary().
raw_line(#message { raw_line = RawLine }) ->
    RawLine.
