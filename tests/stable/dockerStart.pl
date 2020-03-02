% special start file to launch prolog_server in docker environment
:-consult(clioStart).
:-use_module(library(prolog_server)).
% todo: should filter limiting to subnet (first two numbers)
:-set_server_thread.
:-prolog_server(4000,[allow(ip(_,_,_,_))]).

