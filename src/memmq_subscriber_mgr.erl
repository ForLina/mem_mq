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
-module(memmq_subscriber_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([sub/4, get_subscribed/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(SUBSCRIBER, subscriber).
-record(subscriber, {
    name,
    subscribed = []
}).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec sub(Subscriber :: atom(),
          Channel :: atom(),
          HandleMod :: atom(),
          HandleFun :: atom()) -> ok | {error, Reason :: term()}.
sub(Subscriber, Channel, HandleMod, HandleFun) ->
    gen_server:call(?SERVER, {sub, Subscriber, Channel, HandleMod, HandleFun}).

-spec get_subscribed(atom()) -> list().
get_subscribed(Subscriber) ->
    case ets:lookup(?SUBSCRIBER, Subscriber) of
        [] ->
            [];
        [#subscriber{subscribed = List}] ->
            List
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    ets:new(?SUBSCRIBER, [protected, set, named_table, {keypos, 2}]),
    {ok, #state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}).
handle_call({sub, Subscriber, Channel, HandleMod, HandleFun}, _From, State) ->
    Reply = do_sub(Subscriber, Channel, HandleMod, HandleFun),
    {reply, Reply, State};
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_sub(Subscriber, Channel, HandleMod, HandleFun) ->
    CheckItem = [
                    channel_exist,
                    function_exported
                ],
    
    Args = #{
               <<"channel">> => Channel,
               <<"subscriber">> => Subscriber,
               <<"handle_mod">> => HandleMod,
               <<"handle_fun">> => HandleFun
           },
    case do_sub_check(CheckItem, Args) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            do_sub1(Subscriber, Channel, HandleMod, HandleFun)
    end.

do_sub1(Subscriber, Channel, HandleMod, HandleFun) ->
    case get_subscriber(Subscriber) of
        undefined ->
            case memmq_subscriber_sup:start_child([Subscriber]) of
                {ok, _Pid} ->
                    memmq_channel_mgr:add_subscriber(Channel, Subscriber),
                    ets:insert(?SUBSCRIBER, #subscriber{name = Subscriber, subscribed = [{Channel, HandleMod, HandleFun}]}),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        #subscriber{subscribed = Old} = R ->
            New =
                case lists:keyfind(Channel, 1, Old) of
                    false ->
                        [{Channel, HandleMod, HandleFun} | Old];
                    _Tuple ->
                        lists:keyreplace(Channel, 1, Old, {Channel, HandleMod, HandleFun})
                end,
            ets:insert(?SUBSCRIBER, R#subscriber{subscribed = New}),
            ok
    end.

do_sub_check([channel_exist | T], #{<<"channel">> := Channel} = Args) ->
    case whereis(Channel) of
        undefined ->
            {error, no_such_channel};
        _Pid ->
            do_sub_check(T, Args)
    end;
do_sub_check([function_exported], #{
    <<"handle_mod">> := HandleMod,
    <<"handle_fun">> := HandleFun}) ->
    c:l(HandleMod),
    case erlang:function_exported(HandleMod, HandleFun, 1) of
        false ->
            {error, no_handle_function};
        true ->
            ok
    end.

get_subscriber(Subscriber) when is_atom(Subscriber) ->
    case ets:lookup(?SUBSCRIBER, Subscriber) of
        [] ->
            undefined;
        [#subscriber{} = R] ->
            R
    end.