%%%-------------------------------------------------------------------
%% @doc restaurant public API
%% @end
%%%-------------------------------------------------------------------

-module(restaurant_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case restaurant_sup:start_link() of
        {ok, Pid} ->
            io:fwrite("Restaurant supervisor started."),
            {ok, Pid};
        Other ->
            io:fwrite("Restaurant supervisor could not start."),
            {error, Other}
    end.


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
