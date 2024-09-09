/*
note ******************* Generic geographical hierarchies *******************
note doc group geodesc A description of geographic entities.
part name=geodesc; source=pt-acto;
    position=id,dia,mes,ano,fol;
    guaranteed=id;
    also=obs;
    part=geo1,geo2,geo3,geo4,atr,rel

note Idea similar EU NUTS https://ec.eur:-opa.eu/eurostat/web/nuts/background
note doc group geo1 A geographical entity of level 1. Levels geo1 to geo4
     are defined.
note
part name=geo1; source=geoentity;
    position=name,type,id;
    part=geo2,atr,rel

part name=geo2; source=geoentity;
    position=name,type,id;
    part=geo3,atr,rel

part name=geo3; source=geoentity;
    position=name,type,id;
    part=geo4,atr,rel

part name=geo4; source=geoentity;
    position=name,type,id;
    part=atr,rel
*/

/* dynamic loading of mappings works

using module mappings:

        use_module('src/mappings.pl').

    and then consulting a file with mapping definitions

        consult('tests/kleio-home/mappings/geodesc-mapping.pl').

*/

user_mapping geo1 to class geoentity.
user_mapping geo2 to class geoentity.

user_class geo1 super entity table geoentities
  with attributes
        id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
     and
        type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0
	  and
        name column name baseclass name coltype varchar colsize 64 colprecision 0 pkey 0
     and
        obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .
