-module(bump_rebar).

-export([set_release_version/2]).

set_release_version(File, Version) ->
  {ok, Bin} = file:read_file(File),
  Lines = lists:map(fun binary_to_list/1, binary:split(Bin, <<"\n">>, [global])),
  case set_release_version(Version, Lines, false, []) of
    {error, E} -> E;
    Contents ->
      file:write_file(File, [strip(Contents), <<"\n">>])
  end.

set_release_version(Version, [Line | Lines], Found, Acc) ->
  case string:str(Line, "bump marker") > 0 of
    true ->
      NewLine = re:replace(Line, "\"(.+)\".*}.*%%.*bump marker$", Version),
      set_release_version(Version, Lines, true, [NewLine|Acc]);
    false ->
      set_release_version(Version, Lines, Found, [Line | Acc])
  end;

set_release_version(_Version, [], true, Acc) ->
  [ [Line, <<"\n">>] || Line <- lists:reverse(Acc) ];

set_release_version(_Version, [], false, _Acc) ->
  {error, marker_missing}.

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
