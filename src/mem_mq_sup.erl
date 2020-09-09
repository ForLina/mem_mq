%%%-------------------------------------------------------------------
%% @doc mem_mq top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mem_mq_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    Child1 = #{id => 'memmq_channel_mgr',
               start => {'memmq_channel_mgr', start_link, []},
               restart => permanent,
               shutdown => 2000,
               type => worker,
               modules => ['memmq_channel_mgr']},
    
    Child2 = #{id => 'memmq_channel_sup',
               start => {'memmq_channel_sup', start_link, []},
               restart => permanent,
               shutdown => 2000,
               type => supervisor,
               modules => ['memmq_channel_sup']},
    
    Child3 = #{id => 'memmq_subscriber_mgr',
               start => {'memmq_subscriber_mgr', start_link, []},
               restart => permanent,
               shutdown => 2000,
               type => worker,
               modules => ['memmq_subscriber_mgr']},
    
    Child4 = #{id => 'memmq_subscriber_sup',
               start => {'memmq_subscriber_sup', start_link, []},
               restart => permanent,
               shutdown => 2000,
               type => supervisor,
               modules => ['memmq_subscriber_sup']},
    
    {ok, {SupFlags, [Child1, Child2, Child3, Child4]}}.

%% internal functions
