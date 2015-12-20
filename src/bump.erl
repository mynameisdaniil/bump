-module('bump').

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = patch_prv:init(State),
    {ok, State2} = minor_prv:init(State1),
    {ok, State3} = major_prv:init(State2),
    {ok, State3}.
