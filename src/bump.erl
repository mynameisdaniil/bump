-module('bump').

-export([init/1, bump/3]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = patch_prv:init(State),
    {ok, State2} = minor_prv:init(State1),
    {ok, State3} = major_prv:init(State2),
    {ok, State3}.

bump(Module, State, Fun) ->
  {ok, CWD} = file:get_cwd(),
  case bump_git:is_clean(CWD) of
    true ->
      {ok, Version} = bump_rebar:inc_release_version(filename:join(CWD, "rebar.config"), Fun),
      ok = bump_git:add("rebar.config", CWD),
      ok = bump_git:commit(Version, CWD),
      ok = bump_git:tag(Version, CWD),
      {ok, State};
    false ->
      {error, {Module, "Repository is not clean. Commit changes before bumping version."}}
  end.
