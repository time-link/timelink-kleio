:- module(persistence,
    [
    put_value/2,
    put_shared_value/2,
    get_shared_value/2,
    get_value/2,
    set_shared_prop/3,
    get_shared_prop/3,
    del_shared_prop/2,
    get_shared_props/2,
    get_shared_cons/2,
    set_prop/3,has_prop/2,get_prop/3,get_prop/4,del_prop/2,
    del_props/1,get_props/2,get_cons/2,
    add_to_prop/3,show_props/1,show_props/2,
    add_value/2,exists_value/2,has_value/2,has_values/2,remove_value/2,replace_value/3]).


:-use_module(utilities).

/** <module> Persistent variables and atom properties.

## Persistence

These predicates allow the storage of values associated with atoms
(similar to variables in traditional languages)
and properties (similar to dictionaries in traditional languages).

Variables shared across threads are supported.

*/

%% put_value(++Var:atom,++VAL:term) is det.
%
%  Stores VAL associated with Var
%         Val can be retrieved latter on with
%         get_value/2.
% This version thread safe: each thread keeps its
% own values.
% For storage of values between threads see
% put_shared_value/2 and get_shared_value/2
put_value(NAME,VALUE):-
    remember2(NAME,VALUE),!.

%% get_value(+NAME,-VALUE) is det.
%
% Get a value associated with a name.
%
get_value(NAME,VALUE):-recall2(NAME,VALUE),!.

%*****************************************
% Values shared by all threads.
%*****************************************

%%  put_shared_value(+Var,+VAL) is det.
%   Stores a value associated with a name.
%         This version uses the global database for all threads
%         Val can be retrieved latter on by calling
%         get_shared_alue(Var,VAL)
put_shared_value(NAME,VALUE):-
    remember(NAME,VALUE),!.

%% get_shared_value(+Name,?Value) is det.
% Get a shared value associated with a name.
get_shared_value(NAME,VALUE):-recall(NAME,VALUE),!.

%*****************************************
% properties
% global properties, shared by all threads.
%*****************************************

%% set_shared_prop(+Atom,+Prop,+Value) is det.
%
% Store the value of a property of an atom.
% The value is shared among all threads.
%
% @see set_prop/3
%
% ----------------------------------------
set_shared_prop(Atom,Prop,Value):-
    atom(Atom),atom(Prop),
    retractall(prop_(Atom,Prop,_)),
    !,
    assert(prop_(Atom,Prop,Value)).

 set_shared_prop(Atom,Prop,Value):-
    atom(Atom),atom(Prop),
    !,
    assert(prop_(Atom,Prop,Value)).

%% get_shared_prop(+Atom,+Prop,?Value) is det.
%
% Get value of the property of an atom.
 get_shared_prop(Atom,Prop,Value):-
    get_prop_(Atom,Prop,Value).

%% del_shared_prop(+Atom,+Prop) is det.
%  delete shared property of atomn
 del_shared_prop(Atom,Prop) :-
    retractall(prop_(Atom,Prop,_)),!.

 del_shared_props(Atom) :-
    retractall(prop_(Atom,_,_)),!.

%% get_shared_props(+Atom,-Props) is det.
%  Get all the shared properties of an atom
%  "shared" means common to all threads.
 get_shared_props(Atom,Props):-
     setof(P,V^get_prop_(Atom,P,V),Props).

%% get_shared_cons(+Prop,-Atoms) is det.
%  get all the atoms that have a given shared property.
%  "shared" means common to all threads.
% returns an empty list if none.
%
% @see set_shared_property/3
get_shared_cons(Prop,Atoms) :-
    setof(A,V^get_prop_(A,Prop,V),Atoms),!.
get_shared_cons(_,[]) :-!.
%% set_prop(+Atom,+Prop,+Value) is det.
%
% Stores value as the property Prop of Atom.
%
% Storage is local to executing thread.
% Use set_shared_prop/3 for
% properties that need to be shared among threads.
set_prop(Atom,Prop,Value):-
    atomic(Atom),atomic(Prop),
    atom_for_props(Atom,AtomProps),
    (recall2(AtomProps,Dict)   ->
         is_dict(Dict),
         UpdatedDict = Dict.put([Prop=Value]),
         remember2(AtomProps,UpdatedDict)
     ;
         dict_create(Dict,props,[Prop=Value]),
         remember2(AtomProps,Dict)
     ),
     add_value(atoms_with_props_,Atom),
     !.

%% has_prop(+Atom,?Prop) is nondet.
% true if Atom has property Prop.
% If _Prop_ is a variable will give all the property names associated with Atom.
%
% @see set_prop/3
% @see get_props/2
%
has_prop(Atom,Prop):-
     get_props(Atom,Props),
     member(Prop,Props).

%% get_prop(+Atom,+Prop,+Value) is det.
%
% gets the value of property Prop of atom Atom. Value is local to current thread.
%
% @see set_prop/3
%
get_prop(Atom,Prop,Value):-
     atomic(Atom),
     atom_for_props(Atom,AtomProps),
     recall2(AtomProps,Props),
     get_dict(Prop,Props,Value).

get_prop_(Atom,Prop,Value):-clause(prop_(Atom,Prop,Value),true).

% get_prop with default value
get_prop(Atom,Prop,Value, __Default):-
    get_prop(Atom,Prop,Value),
    !.
get_prop(_,_,Default,Default).

%% del_prop(+Atom,+Prop) is det.
% remove Prop from Atom
%
 del_prop(Atom,Prop) :-
    atom(Atom),
    atom_for_props(Atom,AtomProps),
    recall2(AtomProps,Props),
    del_dict(Prop,Props,_,NewProps),
    remember2(AtomProps,NewProps),!.
 del_prop(_,_) :-!. % always succeeds

%% del_props(+Atom) is det.
%
% delete all properties of Atom.
% Suceeds even if Atom has no associated properties.
%

del_props(Atom) :-
    remove_value(atoms_with_props_,Atom),
    atom_for_props(Atom,AtomProps),
    forget2(AtomProps),!.
del_props(_) :-!.      % always succeeds

%% get_props(+Atom,-Props) is det.
% get the list of the names of the properties associated with Atom.
%
% @see has_prop/2
%
get_props(Atom,AllProps):-
    atom_for_props(Atom,AtomProps),
    recall2(AtomProps,Props),
     setof(K,V^get_dict(K,Props,V),AllProps).

%% get_cons(+Prop,-Atoms) is det.
% Return the list of Atoms that have property Prop
get_cons(Prop,Atoms) :-
    has_values(atoms_with_props_,All),
    setof(A,(member(A,All),has_prop(A,Prop)),Atoms).

atom_for_props(A,N):-
     atom_concat(A,'_props_',N),!.

%*************************************************************
% various properties utilities
%*************************************************************

%% add_to_prop(+Atom,+Prop,+Value) is det.
% If the value of Prop is a list adds Value to current Prop of Atom.
% If Prop does not exits, it is created and set to [Prop].
% If Value is already a member of Prop value, does nothing.
% Usefull to deal with with properties with list values.
%
% ### Example
%
%       ?- add_to_prop(iberia,countries,portugal).
%       true.
%       ?- add_to_prop(iberia,countries,spain).
%       true.
%       ?- add_to_prop(iberia,countries,andorra).
%       true.
%       ?- show_props(iberia).
%       iberia
%           countries=[andorra,spain,portugal]
%       true.
%
add_to_prop(O,P,V):-
     get_prop(O,P,OldValue),
     \+ member(V,OldValue),
     set_prop(O,P,[V|OldValue]),!.
% it it is do nothing %
add_to_prop(O,P,V):-
     get_prop(O,P,OldValue),
     member(V,OldValue),!.
% if the property is not there add it %
add_to_prop(O,P,V):-
     \+ get_prop(O,P,_),
     set_prop(O,P,[V]),!.

%% show_props(+Atom) is det.
% displays all the properties of Atom and their values
%

show_props(A):-show_props(A,1).

%% show_props(+Atom,+Ident) is det.
% Like show_props/1 but output is idented Ident spaces.
%
show_props(Atom,T):-
      get_props(Atom,List), % get all the properties of this atom%
      tab(T),write(Atom),nl,
      show_props(Atom,List,T),!.   % print one by one %
show_props(Atom,[Prop|OtherProps],T):-
      get_prop(Atom,Prop,Value),
      N is T+3,
      tab(N),write(Prop),write('='),write(Value),nl,
      show_props(Atom,OtherProps,T),!.
show_props(_,[],_):-!.

% ************************************************************
% manipulating multiple values (List) associated with an atom.
% These predicates extend the put_value / get_value predicates
% for storing variables by making it easy to associate atoms
% with lists of values. This is usefull while trying to work
% without relying too much in dynamic predicates.
% ************************************************************

%% add_value(+Atom,+Value) is det.
% Adds a Value to list of current values stored in Atom
% If Atom has no current value then [Value] is associated to Atom
%

add_value(Atom, Value):-
     recall2(Atom,List),
     ( memberchk(Value,List)->
         true
      ;
        remember2(Atom,[Value|List])),!.

add_value(Atom, Value):-
     remember2(Atom,[Value]),!.

%% exists_value(+Atom,+Value) is det.
% True if Value is contained is the list of values of Atom
% Does not backtrack.
%
% @see has_value/2 for non determinist checks.
%
exists_value(Atom,Value):-
     recall2(Atom,List),
     memberchk(Value,List).

%% has_value(+Atom,?Value) is nondet.
% True if Value is in the list of values of Atom. Can be used to
% backtrack on all the values associated with Atom.
%
% @see exists_value/2 for determinist check of value
has_value(Atom,Value):-
     recall2(Atom,List),
     member(Value,List).

%% has_values(+Atom,-Values) is det.
% retrieve the list of values associated with Atom
%
has_values(Atom,Values):-
     recall2(Atom,Values),!.

%% replace_value(+Atom,+Old,+New) is det.
% replace value Old with New in the list of values of Atom.
% fails if Old is not in Atom values.
%
replace_value(Atom,OldVal,NewVal):-
     recall2(Atom,List),
     memberchk(OldVal,List),!,
     rplc_value(Atom,OldVal,NewVal,List).

rplc_value(Atom,OldVal,NewVal,List):-
     append(H,[OldVal|T],List),
     append(H,[NewVal|T], NewList),
     remember2(Atom, NewList),
     fail.
rplc_value(_,_,_,_):-!.


%% remove_value(+Atom,+Value) is det.
% Remove value from the list of values associated with Atom.
remove_value(Atom,Value):-
     recall2(Atom,List),
     memberchk(Value,List),!,
     rmv_value(Atom,Value,List).

rmv_value(Atom,OldVal,List):-
     append(H,[OldVal|T],List),
     append(H,T, NewList),
     remember2(Atom, NewList),
     fail.
rmv_value(_,_,_):-!.
