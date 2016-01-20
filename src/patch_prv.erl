-module(patch_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, patch).
-define(DEPS, []).
-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).

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
                               {short_desc, "Bump and commit patch version number (e.g. 0.0.X)"},
                               {desc, "npm-like semantic versioning for rebar3"}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  %% io:format("is_clean: ~p\n", [bump_git:is_clean("/home/daniil/projects/my/bump/")]),
  case bump_git:is_clean("/home/daniil/projects/my/bump/") of
    false ->
      ?PRV_ERROR("Repository is not clean. Commit changes before bumping version.");
    true ->
      bump_rebar:set_release_version("/home/daniil/projects/my/bump/rebar.config", "0.0.5"),
      {ok, State}
end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).
