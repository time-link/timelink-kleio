:-module(mappings,[
   mapping/1,
   class/1,
   op(230,fx,mapping),
   op(220,xfx,to),
   op(210,fx,class),
   op(209,xfy,super),
   op(208,xfy,table),
   op(207,xfy,with),
   op(206,fx,attributes),
   op(204,xfy,and),
   op(203,xfy,column),
   op(203,xfy,baseclass),
   op(203,xfy,coltype),
   op(203,xfy,colsize),
   op(203,xfy,colprecision),
   op(203,xfy,pkey)
]).

:-discontiguous(mapping/1).
:-discontiguous(class/1).


mapping geoentity to class geoentity.
class geoentity super entity table geoentities
 with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0
    and
       name column name baseclass name coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping 'authority-register' to class aregister.
class aregister super entity table aregisters
     with attributes
          id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
       and
          date column the_date baseclass date coltype varchar colsize 24 colprecision 0 pkey 0 
       and
          user column user baseclass user coltype varchar colsize 32 colprecision 0 pkey 0 
       and
          name column name baseclass name coltype varchar colsize 254 colprecision 0 pkey 0 
       and
          dbase column dbase baseclass dbase coltype varchar colsize 32 colprecision 0 pkey 0 
     and
        mode column replace_mode baseclass mode coltype varchar colsize 64 colprecision 0 pkey 0
       and
          obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .
mapping rentity to class rentity.
class rentity super entity table rentities
    with attributes
          id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
     and  
        the_class column the_class baseclass the_class coltype varchar colsize 32 colprecision 0 pkey 0
       and
          description column description baseclass name coltype varchar colsize 128 colprecision 0 pkey 0 
       and
          user column user baseclass user coltype varchar colsize 64 colprecision 0 pkey 0 
       and
          status column status baseclass status coltype varchar colsize 64 colprecision 0 pkey 0 
       and
          obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping rperson to class rperson.
     class rperson super rentity table rpersons
       with attributes
             id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
          and
             seq column seq baseclass seq coltype int colsize 10 colprecision 0 pkey 0
          and
             sname column sname baseclass name coltype varchar colsize 128 colprecision 0 pkey 0 
          and
             user column user baseclass user coltype varchar colsize 64 colprecision 0 pkey 0 
          and
             status column status baseclass status coltype varchar colsize 1 colprecision 0 pkey 0 
           and
             sex column sex baseclass sex coltype varchar colsize 1 colprecision 0 pkey 0 
          and
             obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping robject to class robject.
  class robject super rentity table robjects
          with attributes
                id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
             and
                seq column seq baseclass seq coltype int colsize 10 colprecision 0 pkey 0
             and
                sname column sname baseclass name coltype varchar colsize 128 colprecision 0 pkey 0 
             and
                user column user baseclass user coltype varchar colsize 64 colprecision 0 pkey 0 
             and
                status column status baseclass status coltype varchar colsize 1 colprecision 0 pkey 0 
              and
                type column the_type baseclass the_type coltype varchar colsize 32 colprecision 0 pkey 0 
             and
                obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping 'historical-source' to class source.
class source super entity table sources
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       date column the_date baseclass date coltype varchar colsize 24 colprecision 0 pkey 0 
    and
       type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0 
    and
       value column the_value baseclass value coltype varchar colsize 254 colprecision 0 pkey 0 
    and
       loc column loc baseclass loc coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       ref column ref baseclass ref coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       kleiofile column kleiofile baseclass kleiofile coltype varchar colsize 512 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0
    and
       replace column replaces baseclass replace coltype varchar colsize 254 colprecision 0 pkey 0.

mapping 'historical-act' to class act.
class act super entity table acts
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       date column the_date baseclass date coltype varchar colsize 24 colprecision 0 pkey 0 
    and
       type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0 
    and
       loc column loc baseclass loc coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       ref column ref baseclass ref coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping person to class person.
class person super entity table persons
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       name column name baseclass name coltype varchar colsize 128 colprecision 0 pkey 0
    and
       sex column sex baseclass sex coltype char colsize 1 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping object to class object.
class object super entity table objects
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       name column name baseclass name coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping abstraction to class object.

mapping relation to class relation.
class relation super entity table relations
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       date column the_date baseclass date coltype varchar colsize 24 colprecision 0 pkey 0 
    and
       origin column origin baseclass origin coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       destination column destination baseclass destination coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0 
    and
       value column the_value baseclass value coltype varchar colsize 254 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping attribute to class attribute.
class attribute super entity table attributes
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       entity column entity baseclass entity coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       date column the_date baseclass date coltype varchar colsize 24 colprecision 0 pkey 0 
    and
       type column the_type baseclass type coltype varchar colsize 512 colprecision 0 pkey 0
    and
       value column the_value baseclass value coltype varchar colsize 1024 colprecision 0 pkey 0
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping 'group-element' to class group_element.
class group_element super entity table gelement
 with attributes
        id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
       value column the_value baseclass value coltype varchar colsize 254 colprecision 0 pkey 0
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .
     
mapping item to class item.
class item super object table items
 with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
     and
       titulo column titulo baseclass titulo coltype varchar colsize 128 colprecision 0 pkey 0
    and
       resumo column resumo baseclass resumo coltype varchar colsize 2048 colprecision 0 pkey 0 .

mapping proparr to class proparr.
class proparr super object table prop_arrem
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       name column name baseclass name coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       valor column the_value baseclass value coltype varchar colsize 254 colprecision 0 pkey 0 
    and
       condicao column condicao baseclass condicao coltype varchar colsize 1024 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping rmerce to class 'registo-merces'.
class 'registo-merces' super act table rmerces
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       dia column the_day baseclass day coltype numeric colsize 2 colprecision 0 pkey 0 
    and
       mes column the_month baseclass month coltype numeric colsize 2 colprecision 0 pkey 0 
    and
       ano column the_year baseclass year coltype numeric colsize 4 colprecision 0 pkey 0 
    and
       cota column ref baseclass ref coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       merce column merce baseclass merce coltype varchar colsize 1024 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping acta to class acta.
mapping amz to class acta.
class acta super act table actas
   with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
       dia column the_day baseclass day coltype numeric colsize 2 colprecision 0 pkey 0
    and
       mes column the_month baseclass month coltype numeric colsize 2 colprecision 0 pkey 0
    and
       ano column the_year baseclass year coltype numeric colsize 4 colprecision 0 pkey 0
    and
       fol column fol baseclass fol coltype varchar colsize 64 colprecision 0 pkey 0
    and
       resumo column resumo baseclass resumo coltype varchar colsize 1024 colprecision 0 pkey 0
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .


mapping fogo to class household.
class household super object table households
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       dia column the_day baseclass day coltype numeric colsize 2 colprecision 0 pkey 0 
    and
       mes column the_month baseclass month coltype numeric colsize 2 colprecision 0 pkey 0 
    and
       ano column the_year baseclass year coltype numeric colsize 4 colprecision 0 pkey 0 
    and
       loc column loc baseclass loc coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping escritura to class escritura.
class escritura super act table escrituras
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       date column the_date baseclass date coltype varchar colsize 24 colprecision 0 pkey 0 
    and
       type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0 
    and
       loc column loc baseclass loc coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       fol column fol baseclass fol coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       sumario column summary baseclass summary coltype varchar colsize 1024 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping bem to class good.
class good super object table goods
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       name column name baseclass name coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       type column the_type baseclass type coltype varchar colsize 128 colprecision 0 pkey 0 
    and
       loc column loc baseclass loc coltype varchar colsize 128 colprecision 0 pkey 0 
    and
       description column description baseclass description coltype varchar colsize 1024 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping divida to class divida.
class divida super object table dividas
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       valor column the_value baseclass value coltype varchar colsize 254 colprecision 0 pkey 0 
    and
       valorn column valorn baseclass valorn coltype numeric colsize 12 colprecision 2 pkey 0 
    and
       moeda column moeda baseclass moeda coltype varchar colsize 12 colprecision 0 pkey 0 
    and
       prazo column prazo baseclass prazo coltype varchar colsize 254 colprecision 0 pkey 0 
    and
       juro column juro baseclass juro coltype varchar colsize 32 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping siza to class siza.
class siza super object table sizas
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       valor column the_value baseclass value coltype varchar colsize 254 colprecision 0 pkey 0 
    and
       juiz column juiz baseclass juiz coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       escrivao column escrivao baseclass escrivao coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       depositario column depositario baseclass depositario coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       data column the_date baseclass date coltype varchar colsize 24 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping aforamento to class aforamento.
class aforamento super good table aforamentos
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       name column name baseclass name coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0 
    and
       loc column loc baseclass loc coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       description column description baseclass description coltype varchar colsize 1024 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping escambo to class escambo.
class escambo super good table escambos
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       name column name baseclass name coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0 
    and
       loc column loc baseclass loc coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       description column description baseclass description coltype varchar colsize 1024 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

       
mapping caso to class caso.
class caso super object table casos
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
       type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping acus to class acusacoes.
class acusacoes super object table acusacoes
   with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
   and
       idcaso column idcaso baseclass idcaso coltype varchar colsize 64 colprecision 0 pkey 0
   and
       literal column literal baseclass literal coltype varchar colsize 16000 colprecision 0 pkey 0
   and
       origem column origem baseclass origem coltype varchar colsize 16000 colprecision 0 pkey 0
   and
       obs column obs baseclass obs coltype varchar colsize 16000 colprecision 0 pkey 0 .
mapping cartaperdao to class cartaperdao.
class cartaperdao super act table perdoes
 with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
       dia column the_day baseclass day coltype numeric colsize 2 colprecision 0 pkey 0
    and
       mes column the_month baseclass month coltype numeric colsize 2 colprecision 0 pkey 0
    and
       ano column the_year baseclass year coltype numeric colsize 4 colprecision 0 pkey 0
    and
       cota column cota baseclass ref coltype varchar colsize 64 colprecision 0 pkey 0
    and
       local column local baseclass loc coltype varchar colsize 128 colprecision 0 pkey 0
    and
       tipo column the_type baseclass type coltype varchar colsize 128 colprecision 0 pkey 0
    and
       tabeliao column tabeliao baseclass tabeliao coltype varchar colsize 64 colprecision 0 pkey 0
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .

mapping crime to class crime.
class crime super object table cartas_de_perdao
  with attributes
        id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
        dia column the_day baseclass day coltype numeric colsize 2 colprecision 0 pkey 0
    and
        mes column the_month baseclass month coltype numeric colsize 2 colprecision 0 pkey 0
    and
        ano column the_year baseclass year coltype numeric colsize 4 colprecision 0 pkey 0
    and
       forma column forma baseclass forma coltype varchar colsize 64 colprecision 0 pkey 0
    and
       tipo column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0
    and
       subtipo column subtipo baseclass subtipo coltype varchar colsize 32 colprecision 0 pkey 0
    and
       objecto column objecto baseclass objecto coltype varchar colsize 128 colprecision 0 pkey 0
    and
       local column local baseclass loc coltype varchar colsize 32 colprecision 0 pkey 0
    and
       descricao column descricao baseclass descricao coltype varchar colsize 1024 colprecision 0 pkey 0
    and
       tempo column tempo baseclass tempo coltype varchar colsize 32 colprecision 0 pkey 0
    and
       instrumento column instrumento baseclass objecto coltype varchar colsize 32 colprecision 0 pkey 0
    and
       pronuncia column pronuncia baseclass pronuncia coltype varchar colsize 128 colprecision 0 pkey 0
    and
       ocorrencia column ocorrencia baseclass ocorrencia coltype varchar colsize 128 colprecision 0 pkey 0
    and
       situacao column situacao baseclass situacao coltype varchar colsize 128 colprecision 0 pkey 0
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .


mapping perdao to class perdao.
class perdao super abstraction table perdoes
with
        id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
        dia column the_day baseclass day coltype numeric colsize 2 colprecision 0 pkey 0
    and
        mes column the_month baseclass month coltype numeric colsize 2 colprecision 0 pkey 0
    and
        ano column the_year baseclass year coltype numeric colsize 4 colprecision 0 pkey 0
    and
       tipo column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0
    and
       descricao column descricao baseclass descricao coltype varchar colsize 1024 colprecision 0 pkey 0
     and
        obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .


mapping carta to class carta.
class carta super act table cartas
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       date column the_date baseclass date coltype varchar colsize 24 colprecision 0 pkey 0 
    and
       loc column loc baseclass loc coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       resumo column resumo baseclass resumo coltype varchar colsize 1024 colprecision 0 pkey 0 
    and
       refere column refere baseclass refere coltype varchar colsize 1024 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .
       
mapping evento to class evento.
class evento super act table eventos
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       date column the_date baseclass date coltype varchar colsize 24 colprecision 0 pkey 0 
    and
       description column description baseclass description coltype varchar colsize 1024 colprecision 0 pkey 0
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .
       
mapping adenda to class adenda.
class adenda super act table adendas
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1 
    and
       date column the_date baseclass date coltype varchar colsize 24 colprecision 0 pkey 0 
    and
       loc column loc baseclass loc coltype varchar colsize 64 colprecision 0 pkey 0 
    and
       resumo column resumo baseclass resumo coltype varchar colsize 1024 colprecision 0 pkey 0 
    and
       refere column refere baseclass refere coltype varchar colsize 1024 colprecision 0 pkey 0 
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .
       
mapping topico to class topico.
class topico super object table topicos
  with attributes
       id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
       subject column subject baseclass subject coltype varchar colsize 1024 colprecision 0 pkey 0
    and
       description column description baseclass description coltype varchar colsize 1024 colprecision 0 pkey 0
    and
       obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .
