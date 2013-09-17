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
%%% @doc RFC 1459 Utilities
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_rfc1459).

%% API.
-export([parse/1]).

%% Types.
-type message() :: transeo_types:message().
-type prefix() :: transeo_types:prefix().

-include("transeo.hrl").

%% @doc Try parsing an RFC 1459 line.
-spec parse(Data :: binary()) -> {ok, message()} | {error, term()}.
parse(Data) ->
    parse_prefix(Data).

%% @private
-spec parse_prefix(Data :: binary()) -> {ok, message()} | {error, term()}.
parse_prefix(<<$:, Data/binary>>) ->
    case binary:split(Data, <<" ">>) of
        [Prefix, Data2] ->
            parse_command(Prefix, Data2);

        _Otherwise ->
            {error, {invalid_data, Data}}
    end;

parse_prefix(Data) ->
    parse_command(undefined, Data).

%% @private
-spec parse_command(Prefix :: prefix(), Data :: binary()) -> {ok, message()} | {error, term()}.
parse_command(Prefix, Data) ->
    case binary:split(Data, <<" ">>) of
        [Command, ParameterData] ->
            {ok, #message {
                    prefix = Prefix,
                    command = Command,
                    parameters = parse_parameters(ParameterData)
                } };

        [Command] ->
            {ok, #message {
                    prefix = Prefix,
                    command = Command,
                    parameters = []
                } };

        _Otherwise ->
            {error, {invalid_data, Prefix, Data}}
    end.

%% @private
-spec parse_parameters(Data :: binary()) -> [binary()].
parse_parameters(Data) ->
    lists:reverse(parse_parameters(Data, [])).

%% @private
-spec parse_parameters(Data :: binary(), Result :: [binary()]) -> [binary()].
parse_parameters(<<$:, Parameter/binary>>, Result) ->
    [Parameter | Result];

parse_parameters(Data, Result) ->
    case binary:split(Data, <<" ">>) of
        [Parameter, ParameterData] ->
            parse_parameters(ParameterData, [Parameter | Result]);

        [Parameter] ->
            [Parameter | Result];

        _Otherwise ->
            Result
    end.
