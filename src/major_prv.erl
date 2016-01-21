-module(major_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, major).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, bump},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 bump patch|minor|major"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Bump and commit major version number (e.g. X.0.0)"},
            {desc, "npm-like semantic versioning for rebar3"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  bump:bump(?MODULE, State, fun({Major, _Minor, _Patch}) -> {Major + 1, 0, 0} end).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
