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
% the License.-module(foaf_ssl).

-export([verify_user/1]).

-include_lib("public_key/include/public_key.hrl").

verify_user(Cert) ->
  {ok, #'RSAPublicKey'{modulus = CertMod, publicExponent = CertExp}} = extract_pubkey_from(Cert),
  {ok, SAN} = extract_subject_alt_name_from(Cert),
  {uniformResourceIdentifier, URI} = SAN,

  Cmd = "roqet -q -i sparql sparql-query.rq -D " ++ URI,
  
  Result = port:exec(Cmd),
  {_M, _E, RdfMod, RdfExp} = parse_output(Result),
  
  {RdfMod, RdfExp} == {CertMod, CertExp}.

parse_output(Output) -> 
  RdfMPos = string:str(Output, "m="),
  RdfEPos = string:str(Output, "e="),
  RdfModPos = string:str(Output, "mod="),
  RdfExpPos = string:str(Output, "exp="),
  RdfPersonPos = string:str(Output, "person="),
  
  RdfM = string:substr(Output, RdfMPos+2, RdfEPos-RdfMPos-4),
  RdfE = string:substr(Output, RdfEPos+2, RdfModPos-RdfEPos-4),
  RdfMod = hex:string_to_integer(string:substr(Output, RdfModPos+12, RdfExpPos-RdfModPos-16)),
  {RdfExp, []} = string:to_integer(string:substr(Output, RdfExpPos+12, RdfPersonPos-RdfExpPos-16)),
  {RdfM, RdfE, RdfMod, RdfExp}.
  
extract_subject_alt_name_from(Cert) ->
  #'TBSCertificate'{extensions = Extensions} = Cert#'Certificate'.tbsCertificate,
  get_extension('OTP-PUB-KEY':'id-ce-subjectAltName'(), Extensions).

get_extension(Key, [#'Extension'{extnID = Key, extnValue = Value} | _T]) ->
  {ok, [Decoded]} = 'OTP-PUB-KEY':decode('SubjectAltName', list_to_binary(Value)),
  {ok, Decoded};
get_extension(Key, [_H | T]) ->
  get_extension(Key, T);
get_extension(_Key, []) ->
  none.

extract_pubkey_from(Cert) ->
  TBSCert = Cert#'Certificate'.tbsCertificate,
  SPKInfo = TBSCert#'TBSCertificate'.subjectPublicKeyInfo,
  {0, DerKey} = SPKInfo#'SubjectPublicKeyInfo'.subjectPublicKey,
  'OTP-PUB-KEY':decode('RSAPublicKey', DerKey).
