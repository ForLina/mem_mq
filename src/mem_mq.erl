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
    sub/4
]).

%% @doc New channel if doesn't exist
-spec new_channel(atom()) -> {ok, Pid}.
new_channel(ChannelName) ->
    memmq_channel_mgr:new(ChannelName).

%% @doc Publish message to a channel
-spec pub(ChannelName :: atom(), Msg :: any()) ->
    ok | {error, Reason :: term()}.
pub(ChannelName, Msg) ->
    memmq_channel:pub(ChannelName, Msg).

%% @doc Create a subscriber and subscribe specified channel
-spec sub(Subscriber :: atom(),
          Channel :: atom(),
          HandleMod :: atom(),
          HandleFun :: atom()) -> {ok, Pid} | {error, Reason :: term()}.
sub(Subscriber, Channel, HandleMod, HandleFun) ->
    memmq_subscriber_mgr:sub(Subscriber, Channel, HandleMod, HandleFun).