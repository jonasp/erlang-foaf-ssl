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

%%% Purpose: Test if the FOAF+SSL Authentication library is working

-module(foaf_ssl_test).

-export([start/0, init/1]).

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),

    {ok, ListenSock} = ssl:listen(0, mk_opts(listen)),
    {ok, {_, ListenPort}} = ssl:sockname(ListenSock),

    spawn(?MODULE, init, [ListenPort]),

    {ok, AcceptedSock} = ssl:transport_accept(ListenSock),
    ok = ssl:ssl_accept(AcceptedSock),
    io:fwrite("Accept: accepted.~n"),

    %% Get the client certificate from Socket
    {ok, Cert} = ssl:peercert(AcceptedSock, [pkix]),

    %% Verify user with the client certificate
    {ok, WebID} = foaf_ssl:verify_user(Cert),
    io:fwrite("Verified WebID: ~p~n", [WebID]),

    ssl:send(AcceptedSock, "Hello "++WebID),

    ssl:close(AcceptedSock),
    ssl:close(ListenSock),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto).

%% Client connect
init(ListenPort) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, ConnectSock} = ssl:connect(Hostname, ListenPort, mk_opts(connect)),
    {ok, Data} = ssl:recv(ConnectSock, 0),
    io:fwrite("Data received: ~p~n", [Data]),
    ssl:close(ConnectSock).

mk_opts(listen) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    [{active, false},
     {verify, verify_peer},
     {fail_if_no_peer_cert, false},
     {depth, 1},
     {certfile, filename:join([Dir, "server", "cert.pem"])},
     {keyfile, filename:join([Dir, "server", "key.pem"])}];
mk_opts(connect) ->
    [{active, false}, 
     {verify, verify_peer},
     {fail_if_no_peer_cert, false},
     {depth, 1},
     {certfile, "certs/everybody_pub.pem"},
     {keyfile, "certs/everybody_key.pem"}].