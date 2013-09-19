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
%%% @doc Ratbox Protocol Utilities.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_ratbox_utilities).

%% API.
-export([capability_to_atom/1, atom_to_capability/1, decode_capabilities/1]).

%% Types.
-type ratbox_capability() :: transeo_types:ratbox_capability().

%% @doc
%% Convert a given capability to an atom.
-spec capability_to_atom(Data :: binary()) -> ratbox_capability() | {error, term()}.
capability_to_atom(X) ->
    case X of
        <<"QS">> ->
            capability_qs;

        <<"EX">> ->
            capability_ex;

        <<"CHW">> ->
            capability_chw;

        <<"IE">> ->
            capability_ie;

        <<"GLN">> ->
            capability_gln;

        <<"KNOCK">> ->
            capability_knock;

        <<"ZIP">> ->
            capability_zip;

        <<"TB">> ->
            capability_tb;

        <<"ENCAP">> ->
            capability_encap;

        <<"SERVICES">> ->
            capability_services;

        <<"RSFNC">> ->
            capability_rsfnc;

        <<"SAVE">> ->
            capability_save;

        <<"SAVETS_100">> ->
            capability_savets_100;

        _Otherwise ->
            {error, {unknown_ratbox_capability, X}}
    end.

%% @doc
%% Convert a given capability to a binary.
-spec atom_to_capability(ratbox_capability()) -> binary().
atom_to_capability(X) ->
    case X of
        capability_qs ->
            <<"QS">>;

        capability_ex ->
            <<"EX">>;

        capability_ch ->
            <<"CHW">>;

        capability_ie ->
            <<"IE">>;

        capability_gln ->
            <<"GLN">>;

        capability_knock ->
            <<"KNOCK">>;

        capability_zip ->
            <<"ZIP">>;

        capability_tb ->
            <<"TB">>;

        capability_encap ->
            <<"ENCAP">>;

        capability_services ->
            <<"SERVICES">>;

        capability_rsfnc ->
            <<"RSFNC">>;

        capability_save ->
            <<"SAVE">>;

        capability_savets_100 ->
            <<"SAVETS_100">>;

        _Otherwise ->
            {error, {unknown_ratbox_capability_atom, X}}
    end.


%% @doc
%% Decode a binary of capabilities.
-spec decode_capabilities(Data :: binary()) -> [ratbox_capability()].
decode_capabilities(Data) ->
    Tokens = binary:split(Data, <<" ">>, [global]),
    lists:map(fun capability_to_atom/1, Tokens).
