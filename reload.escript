#!/usr/bin/env escript
-export([main/1,
         start/0,
         stop/0,
         loop/0]).

main([]) ->
    start().

% Internal use only

-define(SERVER, beam_reloader). % The registered name of the reload process
-define(CHECK_INTERVAL, 300).   % Milliseconds in between checks for changes


-spec start() -> pid().
start() ->
    case whereis(?SERVER) of
        undefined -> spawn(fun() -> init(?SERVER) end);
        _         -> already_started
    end.

-spec stop() -> 'ok'.
stop() ->
    case whereis(?SERVER) of
        undefined -> not_started;
        _         -> ?SERVER ! stop, ok
    end.

-spec init(atom()) -> 'ok'.
init(Name) ->
    error_logger:info_msg("Starting automatic beam reloader."),
    register(Name, self()),
    loop().

-spec loop() -> 'ok'.
loop() ->
    reload_changed_modules(),
    receive
        stop ->
            error_logger:info_msg("Stopping automatic beam reloader.")
    after ?CHECK_INTERVAL ->
        ?MODULE:loop()
    end.

-spec reload_changed_modules() -> [atom() | {error, atom(), term()}].
reload_changed_modules() ->
    [ reload_module(M) || M <- changed_modules() ].

-spec changed_modules() -> [atom()].
changed_modules() ->
    [ M || {M, F} <- loaded_modules(), is_module_changed(M, F) ].

-spec loaded_modules() -> [{atom(), list()}].
loaded_modules() ->
    [ L || {M, F} = L <- code:all_loaded(), not code:is_sticky(M), is_list(F) ].

-spec is_module_changed(atom(), list()) -> boolean().
is_module_changed(Module, Filename) ->
    LoadedVersion = loaded_version(Module),
    case file_version(Filename) of
        file_not_found -> false;
        LoadedVersion  -> false;
        _              -> true
    end.

-spec file_version(list()) -> [any()] | 'file_not_found'.
file_version(Path) ->
    case beam_lib:version(Path) of
        {ok, {_, FileVersion}} -> FileVersion;
        {error, _, _}          -> file_not_found
    end.

-spec loaded_version(atom()) -> [any()].
loaded_version(Module) ->
    proplists:get_value(vsn, Module:module_info(attributes)).

-spec reload_module(atom()) -> {ok, atom()} | {error, atom(), term()}.
reload_module(Module) ->
    code:purge(Module),
    case code:load_file(Module) of
        {module, _} ->
            error_logger:info_msg("Reloaded module ~s", [Module]),
            Module;
        {error, Error} ->
            {error, Module, Error}
    end.
