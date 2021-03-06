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

-record(message, {
        prefix = undefined :: transeo_types:prefix(),
        command :: binary(),
        parameters = [] :: [binary()],
        raw_line = undefined :: binary()
    }).

%% Internal message for nickname announcements.
-record(nick_message, {
        %% The nickname.
        nickname :: binary(),

        %% The source of the nickname message.
        source :: transeo_types:sid_source(),

        %% Hop count.
        hop_count :: non_neg_integer(),

        %% Timestamp, if available.
        timestamp :: undefined | non_neg_integer(),

        %% Modes.
        %% FIXME: Should be changed to an internal representation.
        modes :: binary(),

        %% Username.
        username :: binary(),

        %% Hostname.
        hostname :: binary(),

        %% Actual Hostname (when spoofed).
        real_hostname :: binary(),

        %% ID, if available.
        id :: binary(),

        %% Realname.
        realname :: binary()
    }).

%% Internal message for end-of-burst announcemcents.
-record(eob_message, {}).

%% Internal message for server announcemcents.
-record(server_message, {
        %% Hostname.
        hostname :: binary(),

        %% Hop count.
        hop_count :: non_neg_integer(),

        %% Source.
        source :: transeo_types:sid_source(),

        %% Description.
        description :: binary()
    }).
