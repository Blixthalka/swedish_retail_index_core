-module(cache_server).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

%%% External API
-export([start_link/0,
         stop/0,
         read/2,
         save/3]).
%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record(item, {timestamp , response }).

-type state() ::  #{binary() => item()}.
-type item() :: #item{}.

% one hour
-define(ALIVE_TIME, 3_600_000).
% 10 mins
-define(CLEAN_MSG_INTERVALL, 600_000).

%%%===================================================================
%%% External API
%%%===================================================================


start_link() ->
    gen_server:start_link(server_ref(), ?MODULE, [], []).


stop() ->
    gen_server:call(server_ref(), stop).


read(Method, Path) ->
    gen_server:call(server_ref(), {read, Method, Path}).


save(Method, Path, Response) ->
    gen_server:call(server_ref(), {save, Method, Path, Response}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Customer :: customer:customer()) -> {ok, state(), hibernate}.
init(_) ->
    timer_send_clean_msg(),
    {ok, #{}, hibernate}.

key(Method, Path) ->
    {Method, Path}.

-spec handle_call(Action :: any(), From :: gen_server:from(), State :: state()) ->
                     {stop, normal, ok, state()}.
handle_call({read, Method, Path}, _From, Cache0) ->
    case maps:get(key(Method, Path), Cache0, undefined) of
        undefined ->
            {reply, undefined, Cache0};
        #item{response = Response} ->
            {reply, {ok, Response}, Cache0}
    end;
handle_call({save, Method, Path, Response}, _From, Cache0) ->
    Cache1 = maps:put(key(Method, Path), new_item(Response), Cache0),
    {reply, {ok, Response}, Cache1};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

-spec handle_cast(Action :: term(), State :: state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
    ?LOG_ERROR("unknown message: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(clean, State :: state()) -> {noreply, state()}.
handle_info(clean, Cache0) ->
    ?LOG_INFO("Cleaning kiid cache"),
    Cache1 =
        maps:filter(fun(Key, #item{timestamp = Timestamp}) ->
                       Now = date_util:now_in_milli_seconds(),
                       case Timestamp + ?ALIVE_TIME > Now of
                           true ->
                               true;
                           false ->
                               ?LOG_INFO("Removing key ~p from cache", [Key]),
                               false
                       end
                    end,
                    Cache0),
    timer_send_clean_msg(),
    {noreply, Cache1}.

-spec terminate(Reason :: types:terminate_reason(), State :: state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
server_ref() ->
    {global, ?MODULE}.

new_item(Response) ->
    #item{response = Response, timestamp = date_util:now_in_milli_seconds()}.

timer_send_clean_msg() ->
    case timer:send_after(?CLEAN_MSG_INTERVALL, self(), clean) of
        {ok, _} ->
            ok;
        {error, _} = Error ->
            ?LOG_ERROR("Error creating start timer ~p", [Error]),
            ok
    end.
