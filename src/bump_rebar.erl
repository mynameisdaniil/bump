-module(bump_rebar).

-export([inc_release_version/2]).

inc_release_version(File, Inc) ->
  {ok, Bin} = file:read_file(File),
  Lines = lists:map(fun binary_to_list/1, binary:split(Bin, <<"\n">>, [global])),
  case inc_release_version(Inc, Lines, false, [], undefined) of
    {error, E} -> E;
    {ok, Contents, {Major, Minor, Patch}} ->
      ok = file:write_file(File, [strip(Contents), <<"\n">>]),
      {ok, io_lib:format("v~B.~B.~B\n", [Major, Minor, Patch])}
  end.

inc_release_version(Inc, [Line | Lines], Found, Acc, Version) ->
  case string:str(Line, "bump marker") > 0 of
    true ->
      OldVersion = parse_version(Line),
      {Major, Minor, Patch} = Inc(OldVersion),
      io:format("v~B.~B.~B\n", [Major, Minor, Patch]),
      Replacement = io_lib:format("\\1v~B.~B.~B\\2", [Major, Minor, Patch]),
      NewLine = re:replace(Line, "(\").+(\".*}.*%%.*bump marker$)", Replacement),
      inc_release_version(Inc, Lines, true, [NewLine|Acc], {Major, Minor, Patch});
    false ->
      inc_release_version(Inc, Lines, Found, [Line | Acc], Version)
  end;

inc_release_version(_Inc, [], true, Acc, Version) ->
  {ok, [ [Line, <<"\n">>] || Line <- lists:reverse(Acc) ], Version};

inc_release_version(_Inc, [], false, _Acc, _Version) ->
  {error, marker_missing}.

parse_version(Line) ->
  case re:run(Line, "\"v?(\\d)\\.(\\d)\\.(\\d)\"", [{capture, all_but_first, list}]) of
    {match, Captured} ->
      list_to_tuple(lists:map(fun list_to_integer/1, Captured));
    _ -> {0, 0, 0}
  end.

strip(<<>>) ->
    <<>>;
strip(B) when is_list(B) ->
    strip(iolist_to_binary(B));
strip(B) when is_binary(B) ->
    LastChar = binary:last(B),
    case whitespace(LastChar) of
        false ->
            B;
        true ->
            strip(binary:part(B, 0, size(B)-1))
    end.

whitespace($ )  -> true;
whitespace($\t) -> true;
whitespace($\r) -> true;
whitespace($\n) -> true;
whitespace(_)   -> false.
