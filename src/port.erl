% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(port).

-export([exec/1, exec/2]).

exec(Cmd) ->
  exec(Cmd, ".").

exec(Cmd, Path) ->
  Port = open_port({spawn, Cmd}, [stream, {cd, Path},  eof, exit_status]),
  lists:flatten(collectOutput(Port, [])).

collectOutput(Port, Output) ->
  receive
    {Port, {data, Data}} ->
      collectOutput(Port, [Output | Data]);
    {Port, {exit_status, _}} ->
      %% return collected output
      Output;
    {Port, Msg} ->
      io:format("receive unknown message from port: ~p~n", [Msg]),
      exit({unknown_message, Msg})
  end.
