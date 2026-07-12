# Kleio notation

## What is Kleio notation?
Kleio notation is a notation for transcribing historical sources.

Kleio notation uses special characters to anotate the text.

Kleio notation files are plain text files with extension `.cli` or `.kleio`.

## Main concepts

### Groups

Groups represent entities in the text (equivalent to entities in a database)

### Elements

Elements represent attributes of the groups (equivalent to fields in a database).

### Aspects

Aspects represent diferent representations of the elements.

There are three aspects:
- core: the main value of the element, it corresponds to the `value` field in the database.
- original: the original wording used for the element in the source. This can be original spelling, abbreviations, units, that are normalzed in the core aspect.
- comment: a comment about the element.

"original" and "comment" aspects are optional.

### Multiple values for elements

An element can have multiple values. Each value can have its own aspects.

This feature is not fully implemented in the current version.

## Special characters

Special characters used are:
The special characters are:

| Character | Meaning | Example |
|-----------|---------| --------|
| `$`       | preceding word is the group name | `person$`|
| `=`| preceding word is an element name | `name=John Doe` |
| `/`| element separator | `name=John Doe/sex=male` |
| `%`| original aspect | `name=John Doe%Jhn Do` |
| `#`| comment aspect | `name=John Doe#This is a comment`|
| `|`| multiple values separator | `nationality=Portuguese|Spanish` |
| `;`| alterative separator for multiple values, set by parameter | `nationality=Portuguese;Spanish` |
| `"`| string delimiter, allows for special characters in string | `url="https://www.example.com/?q=hello world"` |
| `"""`| multi-line string delimiter | `description="""This is a multi-line description. It can span multiple lines."""` |

## Special words

### Group names

Group names must start with a letter and can have digits, hifens and underscores.

### Element names
Element names must start with a letter and can have digits, hifens and underscores.

## Schema (structure) files

A schema file is a text file that defines the structure of a specific kleio notation for the purpose of transcribing a specific type of historical source.

Schema files are text files and can use the origingal kleio notation for defining groups and elements, or can use a more modern syntax based in YAML.

Schema file define:
- groups: which groups are allowed in the notation
- group hierarchy: which groups can be nested inside other groups
- elements: which elements are allowed in each group
- positional named elements: elements that appear in a defined order after the groupname, without the need to specify the element name. E.g. person$John Done  is equivalent to person$name=John Done if element "name" is defined as the first positional element in the group "person"

## White space handling

All withespace is colapsed to a single space, except inside strings delimited by `"` or `"""`.

## Examples

```kleio
        b$b1788.1118/22/1/1788/vigario luis barreto de figueiredo castilho e o reverendo cosme dias ribeiro de coimbra

            n$bernardo/m/obs=diz a margem casais, fl.10v/id=b1788.1118-per1
               ls$datanasc/17880111
```

```kleio
kleio$/translations=1
   fonte$cartas-01/documentação avulsa/obs=vários documentos dispersos em diferentes arquivos

    carta$Carta de renunciação do irmão Sebastião Fernandes/18/1/1600/obs=BA, 49-V-4, AHM C0070,doc 8, fol. 5v/resumo="""
       renuncia a herança, na Companhia de Jesus, na mão do Padre Manuel Dias
          Reitor do Colégio de Macau. "rogando ao d.to P.e que se lhe parecer maior
          ...
          que em Macao se faça instrum.to publico, com forma das constituiçoes do
          Reyno de Portugal"

          "Estas são as notícias q achei do anno de 1600, José Montanha"
          "

       """

         emissor$Sebastião Fernandes%Frz/id=avulso-sebastiao-fernandes
            ls$ordem/Companhia de Jesus
            ls$titulo/irmão
            ls$residente/Nankim, China%Na casa da Compª de Jesus
            ls$naturalidade/Macau, China

            referido$Manuel Aires/id=avulso-manuel-aires
               ls$jesuita-cargo/procuradores do Japão

            referido$Manuel Dias#Sénior ou Junior?/id=avulso-manuel-dias
               ls$jesuita-cargo/reitor do Colégio de Macau

            referido$Miguel dos Santos/id=avulso-sebastiao-fernandes-per3-3
               ls$cargo/governador do Bispado
               ls$titulo/Frei
               ls$ordem/Santo Agostinho

            referido$Paulo de Portugal/id=avulso-sebastiao-fernandes-per4-3
               ls$cargo/Capitão Mor de Macau
               ls$titulo/Dom
```
