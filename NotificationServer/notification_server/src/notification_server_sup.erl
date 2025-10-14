%%%-------------------------------------------------------------------
%% @doc notification_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(notification_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
                 intensity => 1,
                 period => 60},
    
    Listener = #{
        id => cowboy_listener,
        start => {cowboy_listener, start_link, []},
        restart => permanent    
    },

    ChildSpecs = [Listener],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
