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
%%% @doc Utilities.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_utilities).

%% API.
-export([intersperse/2, timestamp/0, binary_to_integer/1]).

%% @doc Intersperse element between the elements of the
%% list.
-spec intersperse(Element :: term(), List :: list()) -> list().
intersperse(_, []) ->
    [];

intersperse(_, [X]) ->
    [X];

intersperse(Element, [X | Rest]) ->
    [X, Element | intersperse(Element, Rest)].

%% @doc Return UNIX epoch timestamp.
-spec timestamp() -> non_neg_integer().
timestamp() ->
    {MegaSeconds, Seconds, _MicroSeconds} = os:timestamp(),
    MegaSeconds * 1000000 + Seconds.

%% @doc Return an integer of the binary.
-spec binary_to_integer(Binary :: binary()) -> integer().
binary_to_integer(Binary) ->
    list_to_integer(binary_to_list(Binary)).
