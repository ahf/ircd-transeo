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
%%% @doc Ircd Protocol Messages.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_ircd_messages).

%% API.
-export([pass/3, server/4, end_of_burst/1, end_of_burst_ack/1]).

%% @doc
%% Create PASS message.
-spec pass(Password :: string(), Version :: binary(), Flags :: binary()) -> iolist().
pass(Password, Version, Flags) ->
    io_lib:format("PASS ~s ~s ~s P", [Password, Version, Flags]).

%% @doc
%% Create SERVER message.
-spec server(Hostname :: string(), HopCount :: non_neg_integer(), Sid :: binary(), Description :: string()) -> iolist().
server(Hostname, HopCount, Sid, Description) ->
    io_lib:format("SERVER ~s ~b ~s :~s", [Hostname, HopCount, Sid, Description]).

%% @doc
%% Create EOB message.
-spec end_of_burst(Sid :: string()) -> iolist().
end_of_burst(Sid) ->
    io_lib:format(":~s EOB", [Sid]).

%% Create EOBACK message.
-spec end_of_burst_ack(Sid :: string()) -> iolist().
end_of_burst_ack(Sid) ->
    io_lib:format(":~s EOBACK", [Sid]).
