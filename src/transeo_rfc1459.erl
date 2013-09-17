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
%%% @doc RFC 1459 Message Encoder and Decoder.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_rfc1459).
-behaviour(transeo_wire_protocol_handler).

%% API.
-export([encode/1, decode/1]).

%% Types.
-type message() :: transeo_types:message().
-type prefix() :: transeo_types:prefix().

-include("transeo.hrl").

%% @doc Decode a set of RFC 1459 lines.
-spec decode(Data :: binary()) -> {ok, [message()], Chunk :: binary()} | {error, term()}.
decode(Data) ->
    decode_lines(Data, []).

%% @doc Encode a message to an iolist().
-spec encode(Message :: message()) -> iolist().
encode(#message { prefix = Prefix, command = Command, parameters = Parameters }) ->
    case Prefix of
        undefined ->
            [Command, encode_parameters(Parameters), <<"\r\n">>];

        _Otherwise ->
            [<<$:, Prefix/binary, " ", Command/binary>>, encode_parameters(Parameters), <<"\r\n">>]
    end.

%% @private
-spec decode_lines(Data :: binary(), Result :: [message()]) -> {ok, [message()], Chunk :: binary()} | {error, term()}.
decode_lines(Data, Result) ->
    case binary:split(Data, <<"\r\n">>) of
        [Chunk] ->
            {ok, lists:reverse(Result), Chunk};

        [Line, Chunk] ->
            case decode_line(Line) of
                {ok, Message} ->
                    decode_lines(Chunk, [Message | Result]);

                {error, _} = Error ->
                    Error
            end
    end.

%% @private
-spec decode_line(Line :: binary()) -> {ok, message()} | {error, term()}.
decode_line(Data) ->
    decode_prefix(Data).

%% @private
-spec decode_prefix(Data :: binary()) -> {ok, message()} | {error, term()}.
decode_prefix(<<$:, Data/binary>>) ->
    case binary:split(Data, <<" ">>) of
        [Prefix, Data2] ->
            decode_command(Prefix, Data2);

        _Otherwise ->
            {error, {invalid_data, Data}}
    end;

decode_prefix(Data) ->
    decode_command(undefined, Data).

%% @private
-spec decode_command(Prefix :: prefix(), Data :: binary()) -> {ok, message()} | {error, term()}.
decode_command(Prefix, Data) ->
    case binary:split(Data, <<" ">>) of
        [Command, ParameterData] ->
            {ok, #message {
                    prefix = Prefix,
                    command = Command,
                    parameters = decode_parameters(ParameterData)
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
-spec decode_parameters(Data :: binary()) -> [binary()].
decode_parameters(Data) ->
    lists:reverse(decode_parameters(Data, [])).

%% @private
-spec decode_parameters(Data :: binary(), Result :: [binary()]) -> [binary()].
decode_parameters(<<$:, Parameter/binary>>, Result) ->
    [Parameter | Result];

decode_parameters(Data, Result) ->
    case binary:split(Data, <<" ">>) of
        [Parameter, ParameterData] ->
            decode_parameters(ParameterData, [Parameter | Result]);

        [Parameter] ->
            [Parameter | Result];

        _Otherwise ->
            Result
    end.

%% @private
-spec encode_parameters(Parameters :: [binary()]) -> [binary()].
encode_parameters(Parameters) ->
    encode_parameters(Parameters, []).

-spec encode_parameters(Parameters :: [binary()], Result :: [binary()]) -> [binary()].
encode_parameters([Parameter], Result) ->
    encode_parameters([], [<<" :", Parameter/binary>> | Result]);

encode_parameters([Parameter | Parameters], Result) ->
    encode_parameters(Parameters, [<<" ", Parameter/binary>> | Result]);

encode_parameters([], Result) ->
    lists:reverse(Result).
