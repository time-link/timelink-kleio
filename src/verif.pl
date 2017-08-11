%*************************************************************************************
% predicados para verificacao do vocabulario de life_story e relacoes
% init_ls_vocab .- inicializa a verificacao de life_story
% init_rel_vocab - inicializa a verificacao de relacoes
% store_ls_vocab(A,V) - armazena um par atributo(A) valor(V)
% store_rel_vocab(T,V) - armazena um par Tipo(T) valor(V)
% list_ls_vocab - faz a lista do vocabulario armazenado desde o ultimo init_ls_vocab
% list_rel_vocab - faz a lista do vocubulario armazenado desde o ultimo init_rel_vocab
%*/
init_ls_vocab:-
 get_prop(ls,atributos,LA),
 member(A,LA),
 del_prop(A,valores),
 fail.
init_ls_vocab:-set_prop(ls,atributos,[]),!.
init_rel_vocab:-
 get_prop(rel,tipos,LA),
 member(A,LA),
 del_prop(A,variantes),
 fail.
init_rel_vocab:-set_prop(rel,tipos,[]),!.
store_ls_vocab(Atrib,V):-
  list_to_a0(Atrib,A), %fazer uma string */
  get_prop(ls,atributos,LA), % ver os atributos ja armazenados */
   % ja la esta este?  se sim continuar se nao acrescentar */
  (member_check(A,LA)->true;LAN=[A|LA],set_prop(ls,atributos,LAN)),
  % ver os valores associados a este atributo, se nao tem inicializar */
  (get_prop(A,valores,Vs)->true;set_prop(A,valores,[V]), Vs=[V]),
  % ver se este valor hja esta registado, se nao acrescentar */
  (member_check(V,Vs)->true;set_prop(A,valores,[V|Vs])),!.
store_rel_vocab(Tipo,V):-
  list_to_a0(Tipo,T), %fazer uma string */
  get_prop(rel,tipos,LA), % ver os atributos ja armazenados */
   % ja la esta este?  se sim continuar se nao acrescentar */
  (member_check(T,LA)->true;LAN=[T|LA],set_prop(rel,tipos,LAN)),
  % ver os valores associados a este atributo, se nao tem inicializar */
  (get_prop(T,variantes,Vs)->true;set_prop(T,variantes,[V]), Vs=[V]),
  % ver se este valor hja esta registado, se nao acrescentar */
  (member_check(V,Vs)->true;set_prop(T,variantes,[V|Vs])),!.

list_ls_vocab:-report([
   nl,writeln('Lista do vocabulario das historias de vida ')]),
   get_prop(ls,atributos,LA),sort(LA,LAS),
   member(A,LAS),
   report([writeln(A)]),
   get_prop(A,valores,LV),sort(LV,LVS),
   member(V,LVS),
   report([write('   '),writelist0ln(V)]),
   fail.
list_ls_vocab:-report([nl,writeln('Fim da lista de vocabulario da historias de vida'),nl]),!.

list_rel_vocab:-
   report([nl,writeln('Lista do vocabulario das relacoes ')]),
   get_prop(rel,tipos,LA),sort(LA,LAS),
   member(A,LAS),
   report([writeln(A)]),
   get_prop(A,variantes,LV),sort(LV,LVS),
   member(V,LVS),
   report([write('   '),writelist0ln(V)]),
   fail.
list_rel_vocab:-report([nl,writeln('Fim da lista de vocabulario das relacoes'),nl]),!.
