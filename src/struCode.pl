% vim: filetype=prolog ts=3
% $Date$ 
% $Author$
% $Id$
% $Log: struCode.pl,v $
% Revision 1.2  2006/05/16 10:52:50  Joaquim
% update dos ficheiros prolog
%
% Revision 1.2  2005/03/10 14:42:27  joaquim
% snapshot commit for purpose of moving the cvs directory.
%
% Revision 1.1  2004/04/08 14:45:24  ltiago
% Source code has passed from Joaquim to Tiago.
% Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
% File build.xml, web.xml and velocity.properties were changed
%
% Revision 1.1.1.1  2001/04/19 23:25:43  jrc
% New Repository.
%
% Revision 1.1.1.1  2001/04/18 23:34:39  jrc
% CVS repository moved from llinux to MacOSX.
%
% Revision 1.3  2001/01/15 18:24:13  jrc
% Correct the behavious of the source/fons parameter in
% element/terminus commands, to make it consistent
% with the behaviour in groups. Now elements keep
% the source/fons parameter are therefore can be used
% in specialization hierarchies like groups.
%
%******************************************************
%  This file contains code called by the DCG grammar that
%    analyses strucutre definitions in the stru syntax window
%    
%    The processing of commands stores temporary information
%    about commands and also creates an internal representation
%    of the structure. This temporary information is stored
%    as properties associated with the command name. Each
%    processed command takes the property status which is
%    set to ok ot notOk by check_complete. The values of the
%    parameters of the nomino command are stored as properties
%    associated to the 'nomino' atom.
% 
%  Predicates:
%  initStru: initialization predicate for structures
%  closeStru: 'clean-up' predicate for structures.
%    
%  init_comand(C) initializes the processing of command C
%    
%  close_command(C,S): ends processing of command and 
%    returns status S, makes completeness checks.
%    
%  set_defaults(Command) sets the default values
%        for parameters of the command and deletes
%       previous values.
%    
%  execParam(C,P,V) takes appropriate action for each param-value pair
%    
%  check_complete(CMD,Result) -- checks if CMD has all the 
%      params it must have it returns OK in result
%       
%    History
%      stable OCT 90.
%      exitus command added 14 November 1990
%      fons parameter stored in group information AUG 97
%      ported to swi prolog 23:38 08-11-1999
%******************************************************
%  %
%*************************************************************
% 
% initStru: initializes the processing of strucutre def
%******************************************************
%  %
initStru(_):-
    setcount(errors,1),
    %put_value(max_errors,12),
   clean_commands,   % clean previous cmds %
   !.
%*************************************************************
% closeStru: finalizes structure analysis.
%*************************************************************
% %
closeStru(_):-
   report([writeln('-----------End of structure --------')]),
!.

%******************************************************
%  init_comand(C) initializes the processing of command C
%    Deletes previous parameters and temporary information
%    for the command and initializes default values for the
%    parameters of the command.
%******************************************************
%  %
init_command(C):-
   del_props(C), 
   set_defaults(C),!.

%******************************************************
%  close_command(C,S): ends processing of command and 
%    returns status S
%
%  nomino: create a structure clause and 
%    clean nomino props
%******************************************************
%  %

close_command(nomino,S):-
    check_complete(nomino,S),
    report([write(' nomino command'),write(S)]),
    create_stru(S), % store structure information%
    del_props(nomino),!.
close_command(pars,S):-
    check_complete(pars,S),
    report([writelistln([' pars',S])]),
    del_props(pars),!.
close_command(terminus,S):-
    check_complete(terminus,S),
    report([writelistln([' terminus',S])]),
    del_props(terminus),!.
close_command(exitus,ok):-!.



%******************************************************
% set_defaults(Command) sets the default values
%        for parameters of the command and deletes
%       previous values from previous commands
%******************************************************
%  %
set_defaults(CMD):-
   \+ on(CMD,[nomino,pars,terminus,exitus]),
   report([write('** Set defaults not implemented for: '),
           write(CMD),nl]),!.

set_defaults(nomino):-
   set_prop(nomino,modus,permanens),
   set_prop(nomino,antiquum,non),
   set_prop(nomino,scribe,[]),
   set_prop(nomino,plures,non),!.

% notes on the defaults of pars and terminus:
%     the defaults are only processed once the value
%     of the nomen parameter is known. Refer to execParam
%
set_defaults(pars):-!.
set_defaults(terminus):-!.
% no defaults for exitus %
set_defaults(exitus):-!.
%***********************************************************
% execParam(C,P,V) takes appropriate action for each param-value pair
%            C = comand name
%            P = paramemter
%            V = value
%
%***********************************************************
%%

%******************************************************
%  execParam for nomino
%******************************************************
%  %
%******************************************************
%  
% a group of parameters have their values stored as properties
%   for the nomino nomen filename %
% primum = document name %
% modus = save or not save the file %
% antiquum = substitute old file? %
% identificatio = first entry of first element is id %

execParam(nomino,Param,ParValue):-
   member_check(Param,[nomen,primum,modus,antiquum,identificatio,plures]),
   set_prop(nomino,Param,ParValue),!.

% the scribe parameter can appear several times %
% scribe = output flags %
execParam(nomino,scribe,Type):-
   add_to_prop(nomino,scribe,Type),!.

% default: unknown nomino param %
execParam(nomino,P,V):-
 \+ member_check(P,
        [nomen,primum,modus,antiquum,identificatio,scribe,plures]),
   error_out(['** Error: nomino param:',P,'=',V]),!.


%*************************************************************
% execParam pars directive
%*************************************************************
%/
  
execParam(pars,nomen,NameList):-
   set_prop(pars,nomen,NameList),
   report([writelist(['Reading pars for:'|NameList])]),
   create_groups(NameList),!.
%*************************************************************
% a group of params have their value stored as properties
% of the groups named in nomen parameter
%%
% needs the nomen to be processed before %
execParam(pars,Param,_):-
   Param \= nomen,
   \+ get_prop(pars,nomen,_), % no nomen, nothing else works %
   error_out('** Nomen parameter needed in pars before other parameters'),!.
 
execParam(pars,Param,Value):-
   member_check(Param,[ordo,sequentia,identificatio,post,prae,locus,signum,
             ceteri,certe,pars,semper,repetitio]),
   get_prop(pars,nomen,Groups),        % get current group names%
   set_groups_prop(Groups,Param,Value),% store the values %    
   !.

%*************************************************************
% execParam pars fons parameter
%%
execParam(pars,fons,Group):-
   
   get_prop(pars,nomen,Groups),        % get current group names%
  set_groups_prop(Groups,fons,Group),  % we store the fons parameter (new AUG 97 %
   copy_fons_g(Group,Groups), %copy the fons group top current ones %
   !.
%*************************************************************
% default: unknown pars param %

execParam(pars,P,V):-
 \+ member_check(P,[nomen,ordo,sequentia,identificatio,post,prae,locus,
             ceteri,certe,fons,signum]),
   report([tab(3),write('Error: unknown pars param:'),
   write(P),write('='),write(V),nl]),!.

%*************************************************************
% execParam terminus directive
%*************************************************************
%/
  
execParam(terminus,nomen,NameList):-
   set_prop(terminus,nomen,NameList),
   report([writelist(['Reading terminus for:'|NameList])]),
   create_elements(NameList),!.
%*************************************************************
% a group of params have their value stored as properties
% of the groups named in nomen parameter
%%
% needs the nomen to be processed before %
execParam(terminus,Param,_):-
   Param \= nomen,
   \+ get_prop(terminus,nomen,_), % no nomen, nothing else works %
   error_out('** Nomen parameter needed in terminus before other parameters'),!.
 
execParam(terminus,Param,Value):-
   member_check(Param,[modus,primum,secundum,ordo,post,prae,pars,sine,signa,
                       forma,ceteri,identificatio,cumule,solum]),
   get_prop(terminus,nomen,Elements), % get current element names%
   set_elements_prop(Elements,Param,Value),% store the values %    
   !.

%*************************************************************
% execParam terminus fons parameter
%%
execParam(terminus,fons,Element):-
   get_prop(terminus,nomen,Elements), % get current elements names%
   set_elements_prop(Elements,fons,Element), % added Jan 2001
	copy_fons_e(Element,Elements), %copy the fons element on top current ones %
   !.
%*************************************************************
% default: unknown terminus param %

execParam(terminus,P,V):-
 \+ member_check(P,[modus,primum,secundum,ordo,post,fons,prae,pars,sine,signa,
                       forma,ceteri,identificatio,cumule,solum]),
   error_out(['Error: unknown terminus param:',P,'=',V]),!.

%**************************************************************
% execParam exitus nomen parameter
%%
execParam(exitus,nomen,N):-
    clause(clioStru(M),true),
    (M = N -> true; error_out('bad nomen parameter of the exitus command'-N)),!.
%*************************************************************
% 
%***********************************************************
% check_complete(CMD,Result) -- checks if CMD has all the 
%      params it must have it returns OK in result
%    if the command has not all the params then it returns
%    notOk and set the property status  to
%    notOk. 
%***********************************************************
%% 
check_complete(CMD,Result):-
   member(CMD,[nomino,pars,terminus]),
   requiredParams(CMD,List),
   missingParam(CMD,List),
   (get_prop(CMD,status,Result); Result = ok),!.
   
check_complete(CMD, notOk):-
   \+ member(CMD,[nomino,pars]),
   report([write('Completeness check not implemented for: '),
   write(CMD),nl]),!.

missingParam(CMD,[Param|OtherParams]):-
   \+ get_prop(CMD,Param,_),
   tab(3),
   error_out(['** Missing parameter for ',CMD,':',Param]),
   set_prop(CMD,status,notOk),
   missingParam(CMD,OtherParams),!.
missingParam(CMD,[Param|OtherParams]):-
   get_prop(CMD,Param,_),
   missingParam(CMD,OtherParams),!.
missingParam(_,[]):-!.

%*************************************************************
% Required params: lists of required parameters for comands
%*************************************************************
% %
requiredParams(nomino,[nomen,primum]).
requiredParams(pars,[nomen]).
requiredParams(terminus,[nomen]).
requiredParams(exitus,[nomen]).
