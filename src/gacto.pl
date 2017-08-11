% text of window:  gacto.exp */
%******************************************************
% gacto.pl Generic translator for Sources that follow
%          the Source-Act-Person model.
%
%  Predicados para lidar com actos avulsos, cuja estrutura
%    e' determinada pelo ficheiro de estrutura do clio.
%
%    E' a primeira tentativa de fazer um tradutor generico
%
%
%   HISTORIA
%     Criado em Agosto de 1997 para processar informacao generica
%     estruturada em funcao da nova estrutura do LN5.
%
% REGISTOS FEITOS AUTOMATICAMENTE
%  Para evitar o registo redundante e repetido de informacao que e' dedut'ivel dos assentos
%    este modulo de traducao produz, para cada baptismo, algumas fichas automaticamente 
%   1) para os pais e maes do baptizado sao geradas as relacoes de 'pai' e 'mae' em relacao
%      aos baptizado.
%   2) se o baptizado nao e' ilegitimo (registado na life-story)
%      e' gerada uma relacao de tipo "parentesco" valor "marido" entre o pai e a mae
%      do baptizado e um estado civil  igual 'c' entre os pais (se os pais forem vivos)
%      ou igual a 'foi c' (se pelo menos um deles esta morto).
%   3) se o baptizado for ilegitimo entao e' gerada uma
%      relacao de tipo "parentesco" e valor 'concubinato' entre
%      os pais da crianca.
%   4) O mesmo raciocinio e' aplicado em relacao aos pais dos pais do baptizado.
%       Por cada grupo pai/mae de baptizado, pai de pai/mae debatpizado
%       e mae de pai/mae de baptizado e para os pais e maes de padrinhos e madrinhas
%       repetem-se os passos 1, 2 e 3.
% 
% ESTRUTURAS TEMPORARIAS DE DADOS CRIADOS PELO MODULO DE TRADUCAO
%  O modulo armazena alguma informacao necessaria ao processamento
%  e 'a verificacao dos dados.
%   VARIAVEIS (manipuladas com as primitivas put_value e get_value)
%     data a data do baptismo construida a partir do valor dos elementos
%          ano, mes e dia, com a formula ANO*10000+mes*100+dia.
%     baptismo - o numero do casamento corrente
%     fonte_tipo: o valor do elemento tipo do grupo devassa
%     fonte_valor: o valor do elemento valor do grupo devassa
%     pessoa: codigo da pessoa corrente
%     pnome: nome da pessoa corrente
%
% 
%   CONJUNTO DE FACTOS (manipulados com assert e retract).
%     rpessoa(ID,NOME) armazena nome e id de cada pessoa referida no documento
%                      (incluindo as pessoas "exteriores" referidas com mesmo_que)
%     mesmo(KEY,MESMO) armazena a referencias do tipo "mesmo_que" KEY e' o id local
%                      e MESMO o id indicado pelo elemento mesmo_que
%     externa(KEY)     armazena as referencias mesmo_que extrior ao documento corrente
%                      e serve so para nas listagens de verificacao assinalar essas pessoas
%     rel_ref(PESSOA-NOME,IDDEST,NOMEDEST) regista relacoes para pessoas ainda nao registadas.
%                    PESSOA-NOME e' a origem e IDDEST-NOMEDEST o destino. Este facto so e' registado
%                    se na altura do processamento da relacao a pessoa destino nao estiver ainda
%                    registada. Quando se registam pessoas os factos rel_ref a elas respeitantes
%                    sao eliminadas. Se nao houver erros no fim do documento nao havera nenhum
%                    facto rel_ref.
%
%     INFORMACAO ASSOCIADA A GERACAO AUTOMATICA DE REGISTOS
%     Para ser possivel gerar os registos automaticos descritos anteriormente sao armazenados
%     durante o processamento de cada baptismo um conjunto de valores. Esses valores sao
%     armazenados como propriedades associados ao tipo de pessoas que ocorrem nos baptismos.
%     A cada pessoa do tipo n, pai, mae, ppai,mpai, pmae,mmae, mad, pad, pmad,mmad,mrmad, sao 
%     associadas as seguintes propriedades:
%        propriedade      valores    obs
%        existe            id       A propriedade esta ausente se a pessoa nao existir no casamento
%                                    coorrente e e' igual ao id da pessoa se estiver presente
%        nome              lista     Nome da pessoa em questao
%        (atributo de ls)  (valor de ls) A informacao de life_story do individuo
%                 
%
%
% PROCEDIMENTOS DE VERIFICACAO
%    Para evitar na medida do possivel que se produzam erros na
%  importacao para reflex, que sao sempre dificeis de recuperar
%  o modulo de traducao procurara detectar o maximo de situacoes
%  de inconsistencia, sobretudo no que diz respeito aos valores
%  que sao responsaveis pela ligacao entre ficheiros.
%    Os procedimentos actualmente existentes sao os seguintes:
%       -  teste de duplicacao de codigo de pessoa: verifica se dentro
%           de um documento exitem duas pessoas com o mesmo codigo.
%       - teste de consistencia nas relacoes entre
%            - relacoes e pessoas destino
%  Os procedimentos de verificacao sao feitos no fim de cada documento
%   e no fim do ficheiro, os resultados sao enviados para o ecran.
%
%    
%    
%******************************************************
%  */
%******************************************************
%  db_init - initializes database
%******************************************************
%  */
db_init:-
    report([
    writeln('** Modulo de traducao Clio->LN5 para actos genericos. versao 1.0b Agosto 97'),
    writeln('   Corresponde ao ficheiro estrutura: gactos.str'),
    writeln('     Coimbra, Agosto de 1997. Versao SWI Novembro de 99')]),
    get_value(data_file,D),
    break_fname(D,Path,_File,Base,_Ext),
    list_to_a0([Path,'/',Base],SOURCE),
    put_value(source,SOURCE),
    put_value(afiles,[]), % lista dos ficheiros usados para os actos */
    concat(SOURCE,'.FON',FFONTE),put_value(ffonte,FFONTE),
    open_file_write(FFONTE),
    concat(SOURCE,'.PES',FPESSOAS),put_value(fpessoas,FPESSOAS),
    open_file_write(FPESSOAS),
    concat(SOURCE,'.LS',FLS),put_value(fls,FLS),
    open_file_write(FLS),
    concat(SOURCE,'.REL',FREL),put_value(frel,FREL),
    open_file_write(FREL),
    erase_temp_data,
    setgensymbol(lsa,0),setgensymbol(rela,0),
    init_ls_vocab,init_rel_vocab,
    !.

%******************************************************
%  db_close - cleans up database
%******************************************************
%  */
db_close:-
    export_defaults,
    verify_data,
    report([writeln('** Traducao terminada.')]),
    get_value(ffonte,FFONTE),close_file(FFONTE),
    get_value(fpessoas,FPESSOAS),close_file(FPESSOAS),
    get_value(fls,FLS),close_file(FLS),
    get_value(frel,FREL),close_file(FREL),
    get_value(afiles,AFS),
    forall(member(_Acto-AFILE,AFS),close_file(AFILE)),!.


get_fname(fontes,F):-get_value(ffonte,F),!.
get_fname(pessoas,F):-get_value(fpessoas,F),!.
get_fname('life-story',F):-get_value(fls,F),!.
get_fname(relacoes,F):-get_value(frel,F),!.
get_fname(Act,F):-get_value(afiles,Fs),member(Act-F,Fs),!.


% os ficheiros associados a cada acto sao dinamicamente criados */
check_act_file(G):-
    get_value(afiles,L),member(G-_F,L),!. % ja existe */
check_act_file(G):- % nao existe */
    get_value(source,SOURCE),
    get_value(afiles,L),
    concat([SOURCE,'.',G],FILE),
    open_file_write(FILE),
    put_value(afiles,[G-FILE,L]),
    writeln(afiles-[G-FILE|L]),!.


%******************************************************
%  db_store  - stores the current group
%    this is the top level predicate that stores
%    the various types of predicates generated from
%    the current data structure
%******************************************************
%  */
db_store:-
    clio_group(G,ID), % get the current group */
    ln5_export(G,ID),
    !.
db_store:-
    error_out(('** ERRO INTERNO: Failure: db_store')),!.

%******************************************************
% ln5_export: exportacao para ln5 generico */
% 
%******************************************************
%  */

ln5_export(fonte,_KEY):-
    export_defaults,
    verify_data,
    get_aspects(core,[id,tipo,ano,localizacao,cota,obs],
                              [ID,TIPO,ANO,LOCALIZACAO,COTA,OBS]),
     list_to_a0(ID,AVALOR),
    put_value(fonte_valor,AVALOR),
    ln5_extra_info([id,tipo,ano,localizacao,cota,obs],EXTRA),
    append(OBS,EXTRA,XOBS),
    rexp('fontes',[TIPO,ID,LOCALIZACAO,COTA,ANO,' ',XOBS]),
    !.
ln5_export(fim,_KEY):-
    export_defaults,
    verify_data,!.

ver_sequencia(DATA):-
    get_value(data,D1),
    (D1 > DATA -> report([writeln('** AVISO: acto fora de ordem cronologica')]);true).
ver_sequencia(_DATA):-
    !.

ln5_export(atr,KEY):-ln5_export(ls,KEY),!.

ln5_export(ls,KEY):-
    get_aspects(core,[atrib,valor,obs],[ATRIB,VALOR,OBS]),
    get_aspects(core,[data],[LSDATA]),
    get_value(data,ACTDATA),
    (LSDATA = [] -> DATA = ACTDATA; 
       (DATA=LSDATA,
        report([writeln('** AVISO: data da historia de vida (ls) usada em vez da data do acto')])
       )
    ),
    get_value(fonte_valor,FONTE),
    get_value(pessoa,PESSOA),
    ln5_extra_info([atrib,valor,obs],EXTRA),
    append(OBS,EXTRA,XOBS),
    store_ls(ATRIB,VALOR),
    store_ls_vocab(ATRIB,VALOR),
    rexp('life-story',[PESSOA, DATA,FONTE,FONTE-KEY,ATRIB,VALOR,XOBS]),
    !.

ln5_export(rel,KEY):-
	   ex_rel(rel,KEY).
store_ls(A,V):-
    getCDAnc(Grupo,_),
    list_to_a0(A,LA), %make atom of atribute name */
    (member_check(Grupo,[referido, referida])->true; set_prop(Grupo,LA,V)).

ex_rel(rel,KEY):-
    get_aspects(core,[tipo,valor,iddest,nomedest,obs],
                              [TIPO,VALOR,IDDEST,NOMEDEST,OBS]),
    get_value(pessoa,PESSOA),
    get_value(pnome,NOME),
    get_value(data,DATA),
    get_value(fonte_valor,FONTE),
     store_rel_vocab(TIPO,VALOR),
    ln5_extra_info([tipo,valor,iddest,nomedest,obs],EXTRA),
    append(OBS,EXTRA,XOBS),
    rexp('relacoes',[FONTE-KEY,DATA,TIPO,VALOR,PESSOA,IDDEST,FONTE,XOBS]),
    list_to_a0(IDDEST,IDDEST2), % take out the element in the IDDEST list */
    % if rel group contains a forward reference to a person then
    %   store the person's id for latter checking */
    (clause(rpessoa(IDDEST2,NOMEDEST),true)-> true;
                  assert(rel_ref(PESSOA-NOME,IDDEST2,NOMEDEST))),
    !.

% aqui e' feita a exportacao dos actos genericos 
ln5_export(Group,ID):-
    clio_group_param(Group,fons,acto),
    %report([writeln('exportacao de acto'-Group)]),
    export_defaults,
    verify_data,
    %report([writeln('default e verify ok'-Group)]),
    clio_group_param(Group,certe,Certe),
    clio_aspects(core,Certe,Values),
    ln5_extra_info(Certe,Extra),
    nl,writelist0(['****** extra info: '|Extra]),nl,
    clio_aspects(core,[dia,mes,ano],[DIA,MES,ANO]),
    (ANO = [] -> ANO2=0;[ANO2]=ANO),
    (DIA = [] -> DIA2=0;[DIA2]=DIA),
    (MES = [] -> MES2=0;[MES2]=MES),
    (DATA is ANO2*10000+MES2*100+DIA2; 
         (error_out(['** ERRO: Problemas com a data em '-G-ID]),fail)),
    put_value(data,DATA),
    put_value(acto,ID),
    put_value(tipo,Group),
    setgensymbol(p,0),
    check_act_file(Group),
    append(Values,[Extra],XValues),
    rexp(Group,XValues),
    !.


ln5_export(G,_):-
    clio_group_param(G,fons,Ancestre),
    member_check(Ancestre,[actorf,actorm,parentem,parentef]),
    get_aspects(core,[nome,obs,mesmo_que],[NOME,OBS,MESMO]),
    get_sex(Ancestre,SEX),
    get_pid(G,ID),
    ln5_extra_info([nome,sexo,obs,mesmo_que,id],EXTRA),
    append(OBS,EXTRA,XOBS),
    ln5_export(pessoa,ID,MESMO,NOME,SEX,XOBS),
    (member(Ancestre,[actorf,actorm])->
           (Func = G,
            export_defaults,
            put_value(actor,G),
            put_value(actor_id,ID));
           (get_value(actor,Ac),list_to_a0([G,'-',Ac],Func))),
    export_funcao(Func,ID),
    !.
    
get_sex(actorm,m):-!.
get_sex(actorf,f):-!.
get_sex(parentem,m):-!.
get_sex(parentef,f):-!.
get_sex(P,?):-!,error_out(['** ERRO INTERNO: Problemas com a inferencia do sexo'-P]).
get_pid(P,I):-get_aspect(core,id,ID),ID \= [],!,list_to_a0(ID,I).
get_pid(P,I):-!,get_value(acto,B),gensymbol(p,PS), I=B-PS.

ln5_export(pessoa,ID,[],NOME,SEXO,OBS):-
    export_pessoa(ID,NOME,SEXO,OBS),!.

ln5_export(pessoa,ID,MESMO,NOME,SEXO,OBS):-
    [] \= MESMO,!,
    list_to_a0(MESMO,MKEY), % fazemos uma string */
    assert(mesmo(ID,MKEY)),% registamos a equivalencia */
    export_mesmo_que(MKEY,NOME).

ln5_export(G,KEY):-tell(user),error_out('** ERRO INTERNO: ln5_export: problems in '-G/KEY),!.

export_funcao(FUNCAO,KEY):- 
     get_value(acto,ACT),
     get_value(data,DATA),
     get_value(pessoa,PESSOA),
     get_value(fonte_valor,FONTE),
     get_value(pnome,NOME),
     set_prop(FUNCAO,existe,PESSOA),
     set_prop(FUNCAO,nome,NOME),
     gensymbol(rla,RA), 
     rexp('relacoes',[FONTE-fa-RA,DATA,'funcao em acto',FUNCAO,PESSOA,ACT,FONTE,'auto']),
     !.

 export_pessoa(KEY,NOME,SEXO,OBS):-
    put_value(pessoa,KEY),
    put_value(pnome,NOME),
    assert(rpessoa(KEY,NOME)),
    retractall(rel_ref(_,KEY,NOME)),
    rexp('pessoas',[NOME,KEY,SEXO,OBS]),
    !.

export_mesmo_que(KEY,NOME):- 
    \+ clause(rpessoa(KEY,_),true),
    put_value(pessoa,KEY),
    put_value(pnome,NOME),
    assert(rpessoa(KEY,NOME)),
    assert(externa(KEY)),
    retractall(rel_ref(_,KEY,NOME)),
    !.

export_mesmo_que(KEY,NOME):-
    clause(rpessoa(KEY,NOME2),true),
    (NOME = NOME2 -> true; 
        report([writelist(['** AVISO: Variacao nominal em mesmo_que: ',KEY,' ']),
                writelist0(NOME),write('/'),writelist0ln(NOME2)])),
    put_value(pessoa,KEY),
    put_value(pnome,NOME),
    !.

%***************************************************
% export_defaults: exporta registos de life-story e
%  relacoes que sao constantes em todos os actos
%****************************************************
%*/
export_defaults:-
    get_prop(n,existe,_),
    couple_defaults,
    parent_defaults,
    erase_temp_data,!.
export_defaults:-
    \+ get_prop(n,existe,_),
    erase_temp_data,!.
export_defaults:-
   tell(user),error_out('** ERRO INTERNO: Problemas no calculo de valores constantes (ls e rel)'),!.
%**********************************************************
% couple_defaults: valores constantes para os casais simples (sem filhos) 
%   que podem ser actor-marido e actor-mulher gera-se uma ls para cada um com
%   atributo  igual a 'ec' (estado civil) e valor igual a 'c'
%  (casado)
% por outro lado um registo 'rel' e' produzido entre o marido
% e a mulher com tipo igual a 'parentesco' e valor igual a 'marido'.
%**********************************************************
%*/
couple_defaults:-!,marido,mulher.

marido:-
    get_value(actor,Ac), % pegar o nome do actor */
    get_value(actor_id,AID), % pegar o nome do actor */
    list_to_a0(['mulher-',Ac],MER),
    get_prop(MER,existe,PNA),
    get_value(data,DATA),
    get_value(acto,ACTO),
    get_value(fonte_valor,FONTE),
    % estado civil =  */
    gensymbol(lsa,ALS),
    rexp('life-story',[AID,DATA,FONTE,ACTO/ALS,ec,c,'auto']),
    gensymbol(lsa,ALS2),
    rexp('life-story',[PNA,DATA,FONTE,ACTO/ALS2,ec,c,'auto']),
    % relacao de parentesco marido gerada automaticamente */
    gensymbol(rela,RELA),
    rexp('relacoes',[ACTO/RELA,DATA,parentesco,marido,AID,
                                    PNA,FONTE,'auto']),
    !.
marido:-!.

mulher:-
    get_value(actor,Ac), % pegar o nome do actor */
    get_value(actor_id,AID), % pegar o nome do actor */
    list_to_a0(['marido-',Ac],MR),
    get_prop(MR,existe,PNA),
    get_value(data,DATA),
    get_value(acto,ACTO),
    get_value(fonte_valor,FONTE),
    % estado civil =  */
    gensymbol(lsa,ALS),
    rexp('life-story',[AID,DATA,FONTE,ACTO/ALS,ec,c,'auto']),
    gensymbol(lsa,ALS2),
    rexp('life-story',[PNA,DATA,FONTE,ACTO/ALS2,ec,c,'auto']),
    % relacao de parentesco marido gerada automaticamente */
    gensymbol(rela,RELA),
    rexp('relacoes',[ACTO/RELA,DATA,parentesco,mulher,AID,
                                    PNA,FONTE,'auto']),
    !.
mulher:-!.

%*************************************************************
% parent_defaults: valores automaticamente gerados para os
%                   pais e avos da crianca e pais dos padrinhos.
%   Para cada 'triangulo' pai-mae-filho:
%     sao geradas as relacoes de 'pai' e 'mae' em relacao
%        ao filho.
%    se o filho nao e' ilegitimo (registado na life-story)
%       e' gerada uma relacao de parentesco marido entre o pai e a mae
%        e um estado civil  igual 'c' (se os pais forem vivos)
%        ou igual a 'foi c' se pelo menos um deles esta morto.
%   se o filho for ilegitimo entao e' gerada uma
%   relacao de tipo parentesco e valor 'concubinato' entre
%    os pais da crianca.
% nota: devia ser feito automaticamente contando os "p's" e os "m's"
%**********************************************************
%        */
parent_defaults:-
    get_value(data,DATA),
    get_value(fonte_valor,FONTE),
    get_value(acto,C),
    get_value(actor,Ac),
    list_to_a0(['pai-',Ac],PAI),
      list_to_a0(['ppai-',Ac],PPAI),
           list_to_a0(['pppai-',Ac],PPPAI),
               list_to_a0(['ppppai-',Ac],PPPPAI),
               list_to_a0(['mpppai-',Ac],MPPPAI),
           list_to_a0(['mppai-',Ac],MPPAI),
      list_to_a0(['mpai-',Ac],MPAI),
          list_to_a0(['pmpai-',Ac],PMPAI),
          list_to_a0(['mmpai-',Ac],MMPAI),
    list_to_a0(['mae-',Ac],MAE),
       list_to_a0(['pmae-',Ac],PMAE),
          list_to_a0(['ppmae-',Ac],PPMAE),
             list_to_a0(['pppmae-',Ac],PPPMAE),
             list_to_a0(['mppmae-',Ac],MPPMAE),
         list_to_a0(['mpmae-',Ac],MPMAE),
       list_to_a0(['mmae-',Ac],MMAE),
          list_to_a0(['pmmae-',Ac],PMMAE),
          list_to_a0(['mmmae-',Ac],MMMAE),
    list_to_a0(['mulher-',Ac],MUL),
    list_to_a0(['pmulher-',Ac],PMUL),
    list_to_a0(['mmulher-',Ac],MMUL),
    pdef(DATA-FONTE-C,Ac,PAI,MAE), % pais da crianca */
        pdef(DATA-FONTE-C,PAI,PPAI,MPAI), % pais do pai */
            pdef(DATA-FONTE-C,PPAI,PPPAI,MPPAI), %pais do pai do pai */
             pdef(DATA-FONTE-C,PPPAI,PPPPAI,MPPPAI), %pais do pai do pai */
           pdef(DATA-FONTE-C,MPAI,PMPAI,MMPAI), %pais do mae do pai */
        pdef(DATA-FONTE-C,MAE,PMAE,MMAE), %pai da mae */
            pdef(DATA-FONTE-C,PMAE,PPMAE,MPMAE), %pais do pai da mae */
               pdef(DATA-FONTE-C,PPMAE,PPPMAE,MPPMAE), %pais do pai da mae */
            pdef(DATA-FONTE-C,MMAE,PMMAE,MMMAE), %pais do mae da mae */
    pdef(DATA-FONTE-C,MUL,PMUL,MMUL), %pais da mulher */
    !.
parent_defaults:-
   get_value(data,D),
   get_value(acto,B),
   report([writelistln(
    ['** ',B-D,'** ERRO INTERNO: Problemas no calculo de valores constantes (pais e filhos)'])]),!.

pdef(D-F-C,Filho,Pai,Mae):-
    get_prop(Filho,existe,FILHO_ID),get_prop(Filho,nome,FILHO),
    get_pai(D-F-C,FILHO,FILHO_ID,Pai,PAI,PAI_ID,PaiVivo), % look if there is a father */
    get_mae(D-F-C,FILHO,FILHO_ID,Mae,MAE,MAE_ID,MaeViva), % look if there is a mother */
    writeln(entrando_check_complete_family(Filho-Pai-Mae,FILHO/FILHO_ID,PAI/PAI_ID,MAE/MAE_ID)),
    check_complete_family(Filho-Pai-Mae,FILHO/FILHO_ID,PAI/PAI_ID,MAE/MAE_ID),
    %writeln(saindo_check_complete_family(Filho-Pai-Mae,FILHO/FILHO_ID,PAI/PAI_ID,MAE/MAE_ID)),
    ((get_prop(Filho,ilegitimo,[sim]);get_prop(Filho,ilegitima,[sim]))->ILEG = sim;ILEG=nao),
    %writeln('ilegitimo?'-ILEG),
    %writeln(entrando-default_casados(D-F-C,PAI,PAI_ID,MAE,MAE_ID,ILEG,PaiVivo,MaeViva)),
    default_casados(D-F-C,PAI,PAI_ID,MAE,MAE_ID,ILEG,PaiVivo,MaeViva),!.

pdef(D-F-C,Filho,Pai,Mae):-
    \+ get_prop(Filho,existe,FILHO_ID),
    /* report([writelistln(
   ['** ',C,D,'** AVISO: Nao tem:',Filho])]),*/ !.

pdef(D-F-C,Filho,Pai,Mae):-
    report([writelistln(
   ['** ',C,D,'** ERRO INTERNO: Problemas no calculo de valores constantes',Filho-Pai-Mae])]),!.

get_pai(D-F-C,FILHO,FILHO_ID,Pai,PAI,PAI_ID,Vivo):-
    get_prop(Pai,existe,PAI_ID), get_prop(Pai,nome,PAI),
    (get_prop(Pai,morto,[antes])->Vivo =nao;Vivo=sim),
    gensymbol(rela,RELA),
    rexp('relacoes',[F-C/RELA,D,parentesco,pai,PAI_ID,
                                    FILHO_ID,F,'auto']),!.
get_pai(D-F-C,FILHO,FILHO_ID,Pai,nao,nao,nil):-
    not(get_prop(Pai,existe,PAI_ID)),!.
    
get_mae(D-F-C,FILHO,FILHO_ID,Mae,MAE,MAE_ID,Vivo):-
    get_prop(Mae,existe,MAE_ID), get_prop(Mae,nome,MAE),
    gensymbol(rela,RELA),
    (get_prop(Mae,morto,[antes])->Vivo =nao;Vivo=sim),
    rexp('relacoes',[F-C/RELA,D,parentesco,mae,MAE_ID,
                                    FILHO_ID,F,'auto']),!.
get_mae(D-F-C,FILHO,FILHO_ID,Mae,nao,nao,nil):-
    not(get_prop(Mae,existe,MAE_ID)),!.

check_complete_family(mad-P-M,_,_,_):-!. % padrinhos nao vale a pena verificar */
check_complete_family(pad-P-M,_,_,_):-!. % madrinhas nao vale a pena verificar */
check_complete_family(F-P-M,Filho,nao/nao,nao/nao):-!. % nenhum dos pais presentes */
check_complete_family(F-P-M,Filho,Pai,nao/nao):- % pai presente, mae ausente */
     Pai \= nao/nao,
     report([writelistln(
    ['** AVISO: Informacao incompleta. Falta a mae de',Filho])]),!.
check_complete_family(F-P-M,Filho,nao/nao,Mae):- % mae presente, pai ausente */
     Mae \= nao/nao,
     report([writelistln(
    ['** AVISO: Informacao incompleta. Falta o pai de',Filho])]),!.
check_complete_family(F-P-M,Filho,Pai,Mae):- % mae presente, pai ausente */
    Mae \= nao/nao, Pai \= nao/nao,!.

% filho legitimo com pais vivos:
% relacao de parentesco 'marido' entre os pais,
% estado civil igual a 'c' para os pais          */
default_casados(D-F-C,nao,nao,nao,nao,_,_,_):- % nao ha pais, termina aqui */
    !.
default_casados(D-F-C,nao,nao,_,_,_,_,_):- % nao ha um dos pais, termina aqui */
    !.
default_casados(D-F-C,_,_,nao,nao,_,_,_):- % nao ha um dos pais, termina aqui */
    !.

default_casados(DATA-FONTE-C,PAI,PAI_ID,MAE,MAE_ID,nao,sim,sim):-
    gensymbol(rela,RELA),
    rexp('relacoes',[FONTE-C/RELA,DATA,parentesco,marido,PAI_ID,
                                    MAE_ID,FONTE,'auto']),
    gensymbol(lsa,LSA1),
    rexp('life-story',[PAI_ID,DATA,FONTE,FONTE-C/LSA1,ec,c,'auto']),
    gensymbol(lsa,LSA2),
    rexp('life-story',[MAE_ID,DATA,FONTE,FONTE-C/LSA2,ec,c,'auto']),
    !.
% filho legitimo com um dos pais mortos (ou ambos)
% relacao foi marido entre o pai e a mae */
% estado civil 'foi c' para os pais */
default_casados(DATA-FONTE-C,PAI,PAI_ID,MAE,MAE_ID,nao,PaiVivo,MaeViva):-
    (PaiVivo=nao;MaeViva=nao),
    gensymbol(rela,RELA),
    rexp('relacoes',[TIPO-FONTE-C/RELA,DATA,parentesco,'foi marido',PAI_ID,
                                    MAE_ID,FONTE,'auto']),
    gensymbol(lsa,LSA1),
    rexp('life-story',[PAI_ID,DATA,FONTE,FONTE-C/LSA1,ec,'foi c','auto']),
    gensymbol(lsa,LSA2),
    rexp('life-story',[MAE_ID,DATA,FONTE,FONTE-C/LSA2,ec,'foi c','auto']),
    !.

% filho ilegitimo relacao concubinato entre os pais */
default_casados(DATA-FONTE-C,PAI,PAI_ID,MAE,MAE_ID,sim,_,_):-
      gensymbol(rela,RELA),
      rexp('relacoes',[FONTE-C/RELA,DATA,parentesco,concubinato,PAI_ID,
                                    MAE_ID,FONTE,'auto']),!.
   
erase_temp_data:-
    get_cons(existe,L),
    member(A,L),
    del_props(A),
				fail.
erase_temp_data:-!.
%*********************************************************
% verify_data: verificacoes da consistencia dos dados
% */
verify_data:- \+ clause(rpessoa(_,_),true),init_ls_vocab,init_rel_vocab,!. % no person, no data */
verify_data:-
    clause(rpessoa(_,_),true),
    report([nl,writeln('************** Relatorio de verificacao de dados ***********')]),
    verify_rel_link,
    verify_pessoas,
    list_persons,
    list_mesmos,
    list_ls_vocab,init_ls_vocab,
    list_rel_vocab,init_rel_vocab,
    report([nl,writeln('************Fim de relatorio de verificacao de dados *******')]),!.
verify_rel_link:-
    report([writeln('Verificacao das referencias em relacoes...')]),
    clause(rel_ref(PI-PN,I,N),true), not( clause(rpessoa(I,N),true)),
    report([
      writelist0(['** ERRO: pessoa inexistente nas relacoes de: ',PI,'-']),
      writelist0(PN),writelist0([': ',I,'-']),writelist0ln(N)]),
    fail.
verify_rel_link:-
    report([writeln('Fim de verificacao de pessoas referidas em relacoes.')]),
    retractall(rel_ref(_,_,_)),!.
verify_pessoas:-
    report([nl,writeln('Verificacao de codigos e nomes de pessoas')]),
    rpessoa(I,N1),rpessoa(I,N2), N1 \= N2,
    report(
     [writelist0(['** ERRO: variacao de nome para o mesmo codigo ',I,'-']),
      writelist0(N1),writelist0ln(['-'|N2])
     ]),
    fail.
verify_pessoas:-
    report([writeln('Fim de verificacao de pessoas.')]),!.
    
list_persons:-
    report([nl,writeln('Pessoas referidas no documento anterior:'),nl]),
%  o bagof seguinte causa um crash no LPA v.3.0 por isso aqui 
%    desistimos de listar alfabeticamente as pessoas
%
    bagof(N-K,rpessoa(K,N),L),
    sort(L,LS),
    member(X-Y,LS), 
%    rpessoa(Y,X), % este predicado substitui as 3 linhas acima */
    report([writelist0(X),writelist0([' (',Y,')'])]),
    (clause(externa(Y),true)->report([writeln(' * EXTERNA')]);report([nl])),
    fail.
list_persons:-report([nl,writeln('Fim da lista de pessoas')]),!.

list_mesmos:-
     report([nl,writeln('Referencias a pessoas ja referidas:')]),
     retract(mesmo(Key,Mesmo)),
     rpessoa(Mesmo,Nome),
     report([writelist0([Key,'=>',Mesmo,':'|Nome])]),
     (clause(externa(Mesmo),true)->report([writeln(' * EXTERNA')]);report([nl])),
     fail.
list_mesmos:-report([writeln('Fim da lista de pessoas ja referidas'),
     writeln('ATENCAO: verifique as referencias a pessoas EXTERNAS')]),
     retractall(rpessoa(_,_)),
     retractall(externa(_)),!.
    

% getRpessoa_nome(CHAVE,NOME)
% nomes sao armazenados junto com as chaves das pessoas em factos rpessoa(CHAVE,NOME).
% este predicado lida tambem com o facto de a chave poder ser respeitante a uma
% pessoa tipo "mesmo".
%*/
getRpessoa_nome(CHAVE,NOME):-
   rpessoa(CHAVE,NOME),!.
getRpessoa_nome(CHAVE,NOME):-
   mesmo(CHAVE,MESMO),
   rpessoa(MESMO,NOME),!.
getRpessoa(CHAVE,_):-
   report([writeln('** ERRO INTERNO: problemas com pessoa ja referida'-CHAVE)]),!.


