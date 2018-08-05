% special start file to launch prolog_server in docker environment
:-consult(clioStart).
:-use_module(library(prolog_server)).
% todo: should filter limiting to subnet (first two numbers)
:-put_value(echo,no).
:-trad_stru_only('gacto2.str').
:-prolog_server(4000,[allow(ip(_,_,_,_))]).
:-set_server_thread.
:-put_value(thread_max_wait_secs,600).
:-version('Timelink kleio server ready. Use tkleio(FILE) to translate.').
:-set_idle.


