%% MIT License
%%
%% Copyright (c) 2020 ForLina
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
-module(mem_mq).

%% API
-export([
    new_channel/1,
    pub/2,
    sub/4, unsub/2
]).

%% @doc New channel if doesn't exist
-spec new_channel(atom()) -> ok.
new_channel(Channel) ->
    memmq_channel_mgr:new_channel(Channel).

%% @doc Publish message to a channel
-spec pub(Channel :: atom(), Msg :: any()) ->
    ok | {error, Reason :: term()}.
pub(Channel, Msg) ->
    memmq_channel:pub(Channel, Msg).

%% @doc Create a subscriber and subscribe specified channel
-spec sub(Subscriber :: atom(),
          Channel :: atom(),
          HandleMod :: atom(),
          HandleFun :: atom()) -> ok | {error, Reason :: term()}.
sub(Subscriber, Channel, HandleMod, HandleFun) ->
    memmq_subscriber_mgr:sub(Subscriber, Channel, HandleMod, HandleFun).

%% @doc Create a subscriber and subscribe specified channel
-spec unsub(atom(), atom()) -> ok | {error, Reason :: term()}.
unsub(Subscriber, Channel) ->
    memmq_channel_mgr:unsub(Subscriber, Channel),
    memmq_subscriber_mgr:unsub(Subscriber, Channel).
    