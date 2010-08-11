## FOAF+SSL authentication library for _Erlang_

This erlang library provides an easy way to authenticate users that have a FOAF+SSL enabled WebID and a corresponding SSL Client Certificate.

# The Problems

* This is my first Erlang project ever and I threw that code together during a train journey. So this is probably the ugliest bit of code you can find on the internet.
* The SPARQL query is done by calling a command line tool and the output parsing is the most dilettantish attempt ever.
* You need to use the new ssl implementation of Erlang which has a few bugs in the stable release and you also need to patch it. A patch is provided against the  R14A beta release of Erlang. I will try to send this patch upstream so hopefully you will not have to apply this patch manually in the future.

# The Good News

* It works for my WebID I created at http://foaf.me. I hope this also works with other WebIDs but I did not test this yet.
