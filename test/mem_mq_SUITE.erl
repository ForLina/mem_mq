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
-module(mem_mq_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [{group, all}].

groups() ->
    [{all, [sequence],
      [t_process_exist,
       t_new_channel,
       t_sub
      ]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(mem_mq),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(mem_mq).

t_process_exist(_Config) ->
    ?assertEqual(true, is_pid(whereis(memmq_channel_mgr))),
    ?assertEqual(true, is_pid(whereis(memmq_channel_sup))),
    ?assertEqual(true, is_pid(whereis(memmq_subscriber_mgr))),
    ?assertEqual(true, is_pid(whereis(memmq_subscriber_sup))).
    
t_new_channel(_Config) ->
    ok = mem_mq:new_channel(a),
    ok = mem_mq:new_channel(a),
    ok = mem_mq:new_channel(b),
    ?assertEqual([a,b], lists:sort(memmq_channel_mgr:all_channels())).

t_sub(_Config) ->
    ok = mem_mq:new_channel(a),
    ok = mem_mq:sub(task, a, memmq_subscriber, test_handle),
    ?assertEqual([{a,memmq_subscriber,test_handle}], memmq_subscriber_mgr:get_subscribed(task)),
    
    ok = mem_mq:unsub(task, a),
    timer:sleep(100),
    ?assertEqual([], memmq_subscriber_mgr:get_subscribed(task)).