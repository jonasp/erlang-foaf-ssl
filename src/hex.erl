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

-module(hex).

-export([string_to_integer/1]).

string_to_integer("") -> 0;
string_to_integer(HexString) ->
  string_to_integer(HexString, 0, 0).
  
string_to_integer("", _N, Out) ->
  Out;
string_to_integer(HexString, N, Out) ->
  {Last, Rest} = get_last_char(HexString),
  NewOut = base_factor(N) * char_to_integer(Last) + Out,
  string_to_integer(Rest, N+1, NewOut).
  
base_factor(N) ->
  base_factor(N, 1).

base_factor(0, Buf) ->
  Buf;
base_factor(N, Buf) ->
  base_factor(N-1, Buf*16).
  
get_last_char(String) ->
  {string:right(String, 1), string:substr(String,1, string:len(String)-1)}.

char_to_integer(HexChar) ->
  case HexChar of
    "A" -> 10;
    "B" -> 11;
    "C" -> 12;
    "D" -> 13;
    "E" -> 14;
    "F" -> 15;
    _ -> {Integer, _} = string:to_integer(HexChar), Integer
  end.
