-module(bump_git).

-export([is_clean/1]).

is_clean(Dir) ->
  case my_exec("git diff --exit-code", Dir) of
    {0, _} -> true;
    _ -> false
  end.

my_exec(Command, Dir) ->
    Port = open_port({spawn, Command}, [stream, eof, hide, exit_status, {cd, Dir}]),
    get_data(Port, []).

get_data(Port, Sofar) ->
    receive
    {Port, {data, Bytes}} ->
        get_data(Port, [Sofar|Bytes]);
    {Port, eof} ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            true
        end,
        receive
        {'EXIT',  Port,  _} ->
            ok
        after 1 ->              % force context switch
            ok
        end,
        ExitCode =
            receive
            {Port, {exit_status, Code}} ->
                Code
        end,
        {ExitCode, lists:flatten(Sofar)}
    end.
