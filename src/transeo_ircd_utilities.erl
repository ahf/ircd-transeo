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
%%% @doc Ircd Protocol Utilities.
%%% @end
%%% ----------------------------------------------------------------------------
-module(transeo_ircd_utilities).

%% API.
-export([create_random_sid/0]).

%% @doc
%% Create random ircd sid.
-spec create_random_sid() -> binary().
create_random_sid() ->
    iolist_to_binary([transeo_random:numeric(), transeo_random:alphanumeric(), transeo_random:alphanumeric(), transeo_random:alphanumeric()]).
