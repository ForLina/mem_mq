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
-module(memmq_subscriber).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(memmq_subscriber_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #memmq_subscriber_state{}} | {ok, State :: #memmq_subscriber_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #memmq_subscriber_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #memmq_subscriber_state{}) ->
                     {reply, Reply :: term(), NewState :: #memmq_subscriber_state{}} |
                     {reply, Reply :: term(), NewState :: #memmq_subscriber_state{}, timeout() | hibernate} |
                     {noreply, NewState :: #memmq_subscriber_state{}} |
                     {noreply, NewState :: #memmq_subscriber_state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #memmq_subscriber_state{}} |
                     {stop, Reason :: term(), NewState :: #memmq_subscriber_state{}}).
handle_call(_Request, _From, State = #memmq_subscriber_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #memmq_subscriber_state{}) ->
    {noreply, NewState :: #memmq_subscriber_state{}} |
    {noreply, NewState :: #memmq_subscriber_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #memmq_subscriber_state{}}).
handle_cast(_Request, State = #memmq_subscriber_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #memmq_subscriber_state{}) ->
    {noreply, NewState :: #memmq_subscriber_state{}} |
    {noreply, NewState :: #memmq_subscriber_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #memmq_subscriber_state{}}).
handle_info(_Info, State = #memmq_subscriber_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #memmq_subscriber_state{}) -> term()).
terminate(_Reason, _State = #memmq_subscriber_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #memmq_subscriber_state{},
                  Extra :: term()) ->
                     {ok, NewState :: #memmq_subscriber_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #memmq_subscriber_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
