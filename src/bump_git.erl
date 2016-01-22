-module(bump_git).

-export([is_clean/1, add/2, commit/2, tag/3]).

is_clean(Dir) ->
  case my_exec("git diff --exit-code", Dir) of
    {0, _} -> true;
    _ -> false
  end.

add(File, Dir) ->
  {0, _} = my_exec(io_lib:format("git add ~s", [File]), Dir),
  ok.

commit(Message, Dir) ->
  {0, _} = my_exec(io_lib:format("git commit -m ~s", [Message]), Dir),
  ok.

tag(Message, Tag, Dir) ->
  {0, _} = my_exec(io_lib:format("git tag -a -m ~s ~s", [Message, Tag]), Dir),
  ok.

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
