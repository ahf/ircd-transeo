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
%%% @doc IRC Mode Utilities.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_modes).

%% API.
-export([parse/1]).

%% Types.
-type modelist() :: transeo_types:modelist().

%% @doc Parse a given mode into a mode list.
-spec parse(RawModes :: binary()) -> modelist() | {error, term()}.
parse(RawModes) ->
    case binary:split(RawModes, <<" ">>, [global, trim]) of
        [Modes | Arguments] ->
            zip(parse_modes(Modes), Arguments);

        _Otherwise ->
            {error, {invalid_input, RawModes}}
    end.

%% @private
parse_modes(Modes) ->
    parse_modes('+', Modes, []).

parse_modes(_, <<>>, ResultModes) ->
    lists:reverse(ResultModes);

parse_modes(_, <<"+", Modes/binary>>, ResultModes) ->
    parse_modes('+', Modes, ResultModes);

parse_modes(_, <<"-", Modes/binary>>, ResultModes) ->
    parse_modes('-', Modes, ResultModes);

parse_modes(Op, <<Mode:8, Modes/binary>>, ResultModes) ->
    parse_modes(Op, Modes, [{Op, list_to_atom([Mode])} | ResultModes]).

%% @private
zip(Modes, Arguments) ->
    zip(Modes, Arguments, []).

zip([], [], Result) ->
    lists:reverse(Result);

zip([Mode | Modes], [], Result) ->
    zip(Modes, [], [Mode | Result]);

zip([], Arguments, _) ->
    {error, {insufficient_mode_number, Arguments}};

zip([{Op, Mode} | Modes], [Argument | Arguments], Result) ->
    zip(Modes, Arguments, [{Op, Mode, Argument} | Result]).
