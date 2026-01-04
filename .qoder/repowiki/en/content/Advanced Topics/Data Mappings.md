# Data Mappings

<cite>
**Referenced Files in This Document**   
- [mappings.pl](file://src/mappings.pl)
- [persistence.pl](file://src/persistence.pl)
- [apiTranslations.pl](file://src/apiTranslations.pl)
- [clioPP.pl](file://src/clioPP.pl)
- [dataCode.pl](file://src/dataCode.pl)
- [gactoxml.pl](file://src/gactoxml.pl)
- [dataDictionary.pl](file://src/dataDictionary.pl)
- [externals.pl](file://src/externals.pl)
- [geodesc-mapping.pl](file://tests/kleio-home/mappings/geodesc-mapping.pl)
- [person-mapping.yml](file://tests/kleio-home/mappings/person-mapping.yml)
- [sample-mapping.yml](file://tests/kleio-home/mappings/sample-mapping.yml)
</cite>

## Table of Contents
1. [Introduction](#introduction)
2. [Mapping DSL Syntax](#mapping-dsl-syntax)
3. [Field Specifications](#field-specifications)
4. [Class Hierarchies and Inheritance](#class-hierarchies-and-inheritance)
5. [Core Entity Mappings](#core-entity-mappings)
6. [Complex Historical Entity Mappings](#complex-historical-entity-mappings)
7. [Mapping Integration with Translation Process](#mapping-integration-with-translation-process)
8. [Extending the Mapping System](#extending-the-mapping-system)
9. [Common Issues and Solutions](#common-issues-and-solutions)
10. [Best Practices](#best-practices)

## Introduction

The data mapping system in the TimeLink-Kleio project defines how Kleio data structures are transformed into database schemas. This system uses a domain-specific language (DSL) implemented in Prolog to establish the relationship between Kleio groups (source-oriented model) and database entities (person-oriented model). The mappings.pl file serves as the central configuration for this transformation process, defining how various historical entities are represented in the database.

The mapping system enables the translation of Kleio's hierarchical, text-based format into a structured relational database schema. This transformation is essential for enabling efficient querying, analysis, and integration of historical data. The system supports a wide range of entity types, from basic concepts like persons and objects to complex historical records like letters of pardon (cartaperdao) and household records (fogo).

This documentation provides a comprehensive guide to the mapping system, covering the DSL syntax, field specifications, class hierarchies, and practical examples of how different entity types are mapped to database tables.

**Section sources**
- [mappings.pl](file://src/mappings.pl#L1-L518)

## Mapping DSL Syntax

The mapping system uses a custom DSL with specific operators and syntax to define the relationship between Kleio groups and database entities. The DSL is implemented using Prolog operators that create a readable, domain-specific syntax.

The core syntax elements include:

- `mapping/to`: Defines the mapping between a Kleio group and a database class
- `class/super/table/with`: Defines class properties including inheritance and table mapping
- `attributes/and/column/baseclass/coltype/colsize/pkey`: Specifies field-level details

The DSL is defined through Prolog operator declarations that establish the precedence and associativity of the mapping syntax:

```prolog
:- op(230,fx,mapping)
:- op(220,xfx,to)
:- op(210,fx,class)
:- op(209,xfy,super)
:- op(208,xfy,table)
:- op(207,xfy,with)
:- op(206,fx,attributes)
:- op(204,xfy,and)
:- op(203,xfy,column)
:- op(203,xfy,baseclass)
:- op(203,xfy,coltype)
:- op(203,xfy,colsize)
:- op(203,xfy,colprecision)
:- op(203,xfy,pkey)
```

A basic mapping follows this pattern:

```prolog
mapping person to class person.
class person super entity table persons
  with attributes
    id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
    name column name baseclass name coltype varchar colsize 128 colprecision 0 pkey 0
    and
    sex column sex baseclass sex coltype char colsize 1 colprecision 0 pkey 0
    and
    obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0.
```

This syntax creates a fluent interface where mappings read naturally as "mapping person to class person" and "class person super entity table persons with attributes...".

The system also supports YAML-based mapping definitions, providing an alternative format for defining mappings:

```yaml
- mapping: { name: person, class: person }
- class:
    name: person
    extends: person
    table: entities
    description: >
      This class represents a person.
      It is a subclass of the class entity.
    attributes:
      - { name: id, column: id, class: id, type: string, size: 64, pkey: 1 }
      - { name: name, column: id, class: name, type: string, size: 128, pkey: 0 }
```

**Section sources**
- [mappings.pl](file://src/mappings.pl#L1-L518)
- [person-mapping.yml](file://tests/kleio-home/mappings/person-mapping.yml#L1-L15)
- [sample-mapping.yml](file://tests/kleio-home/mappings/sample-mapping.yml#L1-L24)

## Field Specifications

The mapping system provides detailed field specifications that define how individual attributes are transformed from Kleio groups to database columns. Each field specification includes several key properties that control the database schema generation and data transformation process.

### Core Field Attributes

The primary field specification attributes include:

- **column**: Maps a Kleio element to a database column name
- **baseclass**: Specifies the base class of the attribute
- **coltype**: Defines the database column type (e.g., varchar, numeric, char)
- **colsize**: Specifies the size/length of the column
- **colprecision**: Defines the precision for numeric types
- **pkey**: Indicates if the field is part of the primary key (1) or not (0)

For example, in the person mapping:

```prolog
id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
```

This specification indicates that the "id" element in the Kleio group maps to an "id" column in the database, with a varchar type, size 64, and it serves as the primary key.

### Data Type Support

The mapping system supports various database column types:

- **varchar**: Variable-length character strings (used for text fields)
- **char**: Fixed-length character strings (used for single-character fields like sex)
- **numeric**: Numeric values with specified precision (used for date components)
- **int**: Integer values (used for sequence numbers)

The colsize parameter controls the maximum length of character fields, while colprecision is used for numeric fields to specify decimal places. For example, the household mapping uses numeric types for date components:

```prolog
dia column the_day baseclass day coltype numeric colsize 2 colprecision 0 pkey 0
mes column the_month baseclass month coltype numeric colsize 2 colprecision 0 pkey 0
ano column the_year baseclass year coltype numeric colsize 4 colprecision 0 pkey 0
```

### Primary Key Configuration

The pkey attribute is crucial for defining the database schema's primary key structure. A value of 1 indicates the field is part of the primary key, while 0 indicates it is not. In most mappings, the "id" field serves as the primary key:

```prolog
id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
```

Some entities may have composite primary keys, though the current mappings primarily use single-field primary keys. The primary key configuration ensures data integrity and enables efficient indexing and querying.

**Section sources**
- [mappings.pl](file://src/mappings.pl#L1-L518)

## Class Hierarchies and Inheritance

The mapping system implements a class hierarchy that enables code reuse and consistent data modeling across different entity types. This inheritance system follows the pattern of a base "entity" class that serves as the foundation for all other entity types.

### Base Entity Class

The "entity" class serves as the root of the inheritance hierarchy, providing common attributes that are shared across all entity types:

- id: Unique identifier (primary key)
- obs: Observations or notes field
- Other common metadata fields

All other classes inherit from this base entity, ensuring consistency in fundamental data structure.

### Inheritance Pattern

The inheritance system uses the "super" keyword to establish parent-child relationships between classes:

```prolog
class person super entity table persons
class object super entity table objects
class relation super entity table relations
```

This pattern allows specialized classes to inherit common attributes from their parent classes while adding their own specific attributes. For example, the "person" class inherits the id and obs fields from "entity" while adding name and sex fields specific to persons.

### Multiple Inheritance Examples

The system demonstrates various inheritance patterns:

1. **Direct inheritance from entity**:
```prolog
class person super entity table persons
class object super entity table objects
```

2. **Multi-level inheritance**:
```prolog
class rperson super rentity table rpersons
class rentity super entity table rentities
```

3. **Specialized inheritance for historical records**:
```prolog
class cartaperdao super act table perdoes
class escritura super act table escrituras
```

The inheritance system enables the creation of specialized entity types while maintaining a consistent data model. This approach reduces redundancy and ensures that changes to common attributes (like the id field) are automatically propagated to all inheriting classes.

The class hierarchy also supports the concept of "abstraction" where certain mappings represent abstract concepts rather than concrete entities:

```prolog
mapping abstraction to class object.
mapping perdao to class perdao.
class perdao super abstraction table perdoes
```

This allows for flexible modeling of complex historical concepts that may not fit neatly into traditional entity categories.

**Section sources**
- [mappings.pl](file://src/mappings.pl#L1-L518)
- [dataDictionary.pl](file://src/dataDictionary.pl#L564-L597)

## Core Entity Mappings

The mapping system defines transformations for fundamental entity types that form the backbone of the historical data model. These core entities include persons, objects, relations, and various administrative records.

### Person Mapping

The person entity is one of the most fundamental mappings, transforming Kleio's person groups into database records:

```prolog
mapping person to class person.
class person super entity table persons
  with attributes
    id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
    name column name baseclass name coltype varchar colsize 128 colprecision 0 pkey 0
    and
    sex column sex baseclass sex coltype char colsize 1 colprecision 0 pkey 0
    and
    obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0.
```

This mapping transforms Kleio person groups (e.g., "n$John Smith") into database records in the "persons" table. The name field has a larger size (128) to accommodate full names, while the sex field uses a single character to store gender information.

### Object Mapping

Objects represent tangible or intangible items in the historical record:

```prolog
mapping object to class object.
class object super entity table objects
  with attributes
    id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
    name column name baseclass name coltype varchar colsize 64 colprecision 0 pkey 0
    and
    type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0
    and
    obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0.
```

The object mapping includes a type field to categorize different kinds of objects, enabling classification and filtering of object records.

### Relation Mapping

Relations capture connections between entities:

```prolog
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
    obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0.
```

The relation mapping includes origin and destination fields to establish connections between entities, with type and value fields to describe the nature of the relationship.

### Administrative Record Mappings

The system also includes mappings for various administrative records:

```prolog
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
    obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0.
```

These mappings transform administrative records into database entities, preserving important metadata about data management and provenance.

**Section sources**
- [mappings.pl](file://src/mappings.pl#L1-L518)

## Complex Historical Entity Mappings

The mapping system handles complex historical entities that require specialized data structures to capture their unique characteristics. These mappings demonstrate the flexibility of the system in accommodating diverse historical record types.

### Cartaperdao (Letters of Pardon) Mapping

The cartaperdao mapping handles letters of pardon, which are complex historical documents with specific attributes:

```prolog
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
    obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0.
```

This mapping captures the specific details of letters of pardon, including the notary (tabeliao) who issued the document and the specific type of pardon granted. The use of numeric types for date components allows for precise date arithmetic and sorting.

### Fogo (Household) Mapping

The fogo mapping handles household records, which are essential for understanding social and economic structures:

```prolog
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
    obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0.
```

The household mapping inherits from the object class, treating households as a specialized type of object. It includes date components to track when households were recorded, enabling temporal analysis of household composition and movement.

### Escritura (Legal Document) Mapping

The escritura mapping handles various types of legal documents:

```prolog
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
    obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0.
```

This mapping includes a summary field with increased size (1024) to accommodate detailed descriptions of legal documents. The fol (folio) field tracks the physical location of documents in archival collections.

### Geographical Entity Mapping

The system also supports geographical hierarchies through specialized mappings:

```prolog
mapping geo1 to class geo1.
class geo1 super entity table geoentities
  with attributes
    id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
    type column the_type baseclass type coltype varchar colsize 32 colprecision 0 pkey 0
    and
    name column name baseclass name coltype varchar colsize 64 colprecision 0 pkey 0
    and
    obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0.
```

This pattern is repeated for geo2, geo3, and geo4, creating a hierarchical geographical classification system that can represent administrative divisions at multiple levels.

**Section sources**
- [mappings.pl](file://src/mappings.pl#L1-L518)
- [geodesc-mapping.pl](file://tests/kleio-home/mappings/geodesc-mapping.pl#L1-L52)

## Mapping Integration with Translation Process

The mapping system is tightly integrated with the Kleio translation process, which transforms source files into structured database records. This integration occurs through several key components that work together to apply mappings during translation.

### Translation Workflow

The translation process begins with the apiTranslations.pl module, which handles API requests for translation operations. When a translation is requested, the system:

1. Resolves the source file path
2. Determines the appropriate structure file
3. Loads user-defined mappings if available
4. Spawns translation jobs
5. Processes the results

The translation process checks for user-defined mappings, allowing for customization of the mapping system:

```prolog
% TODO: check_user_mappings(TokenInfo) % load user defined mappings if any
% TODO: check_user_irules(TokenInfo) % load usr defined inference rules if any
```

### Mapping Detection and Application

The gactoxml.pl module contains the core logic for applying mappings during translation. It uses the isNewMappingMode predicate to detect whether the new Prolog-based mapping system is in use:

```prolog
isNewMappingMode:-
  clause(mapping _ to class _, _),!.
```

This predicate checks for the existence of mapping clauses, enabling the system to distinguish between the new Prolog-based mappings and older pseudo-group based mappings.

The groupToClass predicate is responsible for determining the appropriate class for a Kleio group:

```prolog
groupToClass(Group,Class,Super,Table) :-
  mapping Group to class Class,
  class Class super Super table Table with attributes _,!.
```

This function looks up the mapping for a given group and returns the corresponding class, superclass, and table information.

### Persistence and State Management

The persistence.pl module provides the underlying storage mechanism for the mapping system. It implements predicates for storing and retrieving values and properties, with support for both thread-local and shared storage:

```prolog
put_value(NAME,VALUE):-remember2(NAME,VALUE),!.
get_value(NAME,VALUE):-recall2(NAME,VALUE),!.
put_shared_value(NAME,VALUE):-remember(NAME,VALUE),!.
get_shared_value(NAME,VALUE):-recall(NAME,VALUE),!.
```

This persistence system allows the mapping configuration to be stored and accessed throughout the translation process, ensuring consistency across different stages of processing.

### Dynamic Mapping Loading

The system supports dynamic loading of mapping definitions, allowing for runtime configuration changes:

```prolog
/* dynamic loading of mappings works 

using module mappings:

        use_module('src/mappings.pl').

    and then consulting a file with mapping definitions

        consult('tests/kleio-home/mappings/geodesc-mapping.pl').  
*/
```

This capability enables the system to load additional mappings at runtime, providing flexibility for specialized processing requirements.

**Section sources**
- [apiTranslations.pl](file://src/apiTranslations.pl#L1-L200)
- [gactoxml.pl](file://src/gactoxml.pl#L2048-L2316)
- [persistence.pl](file://src/persistence.pl#L1-L200)
- [clioPP.pl](file://src/clioPP.pl#L1-L200)

## Extending the Mapping System

The mapping system is designed to be extensible, allowing researchers to customize the data model for their specific research needs. This section outlines the methods for extending the system with new entity types and custom field configurations.

### Adding New Entity Types

To add a new entity type, create a mapping definition that follows the established pattern:

```prolog
mapping new_entity_type to class new_entity_class.
class new_entity_class super entity table new_entities
  with attributes
    id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
    name column name baseclass name coltype varchar colsize 128 colprecision 0 pkey 0
    and
    description column description baseclass description coltype varchar colsize 1024 colprecision 0 pkey 0
    and
    obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0.
```

Key considerations when adding new entity types:

1. Choose a unique name for the mapping
2. Select an appropriate superclass based on the entity's characteristics
3. Define a descriptive table name
4. Include the standard id and obs fields
5. Add entity-specific attributes as needed

### Customizing Field Configurations

Field configurations can be customized to meet specific research requirements:

1. **Adjust column sizes**: Increase colsize for fields that need to store longer text
2. **Change data types**: Use numeric types for quantitative data, varchar for text
3. **Modify precision**: Set colprecision for numeric fields requiring decimal places
4. **Configure primary keys**: Designate appropriate fields as primary keys

For example, to create a specialized person mapping with additional fields:

```prolog
mapping detailed_person to class detailed_person.
class detailed_person super person table detailed_persons
  with attributes
    id column id baseclass id coltype varchar colsize 64 colprecision 0 pkey 1
    and
    name column name baseclass name coltype varchar colsize 128 colprecision 0 pkey 0
    and
    birth_date column birth_date baseclass birth_date coltype varchar colsize 24 colprecision 0 pkey 0
    and
    death_date column death_date baseclass death_date coltype varchar colsize 24 colprecision 0 pkey 0
    and
    occupation column occupation baseclass occupation coltype varchar colsize 64 colprecision 0 pkey 0
    and
    wealth_level column wealth_level baseclass wealth_level coltype numeric colsize 10 colprecision 2 pkey 0
    and
    obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0.
```

### Using YAML for Mapping Definitions

The system supports YAML-based mapping definitions as an alternative to Prolog syntax:

```yaml
- mapping: { name: custom_entity, class: custom_entity }
- class:
    name: custom_entity
    extends: entity
    table: custom_entities
    description: >
      Custom entity for specialized research.
    attributes:
      - { name: id, column: id, class: id, type: string, size: 64, pkey: true }
      - { name: title, column: title, class: title, type: string, size: 256, pkey: false }
      - { name: content, column: content, class: content, type: text, size: 32768, pkey: false }
      - { name: created_date, column: created_date, class: date, type: string, size: 24, pkey: false }
```

This format may be more accessible for users who are not familiar with Prolog syntax.

### Dynamic Mapping Loading

Mappings can be loaded dynamically at runtime using the consult predicate:

```prolog
consult('path/to/custom_mappings.pl').
```

This allows researchers to load specialized mappings for specific projects without modifying the core system.

### Best Practices for Extension

When extending the mapping system:

1. Follow consistent naming conventions
2. Document new mappings thoroughly
3. Test mappings with sample data
4. Consider backward compatibility
5. Use version control for mapping files
6. Validate mappings against actual data

**Section sources**
- [mappings.pl](file://src/mappings.pl#L1-L518)
- [person-mapping.yml](file://tests/kleio-home/mappings/person-mapping.yml#L1-L15)
- [sample-mapping.yml](file://tests/kleio-home/mappings/sample-mapping.yml#L1-L24)

## Common Issues and Solutions

This section addresses common issues encountered when working with the mapping system and provides solutions for troubleshooting and resolving these problems.

### Mapping Not Applied

**Issue**: A defined mapping is not being applied during translation.

**Solution**: Verify that:
1. The mapping syntax is correct and properly formatted
2. The mapping file is being loaded by the system
3. The Kleio group name matches the mapping definition exactly
4. There are no conflicting mappings for the same group

Check the translation logs for any error messages related to mapping resolution.

### Field Mapping Errors

**Issue**: Fields are not being mapped correctly or data is being truncated.

**Solution**: 
1. Verify that colsize is sufficient for the data being stored
2. Check that coltype matches the data type (e.g., numeric for numbers, varchar for text)
3. Ensure that the column name in the mapping matches the database schema
4. Validate that the baseclass specification is correct

For example, if date components are being truncated, increase the colsize:

```prolog
dia column the_day baseclass day coltype numeric colsize 3 colprecision 0 pkey 0
```

### Inheritance Issues

**Issue**: Inherited fields are not appearing in the database table.

**Solution**:
1. Verify that the superclass is correctly specified in the "super" clause
2. Check that the superclass definition includes the expected attributes
3. Ensure that the inheritance chain is complete (e.g., if class A inherits from B, and B inherits from C, verify both links)

### Primary Key Conflicts

**Issue**: Primary key violations during data insertion.

**Solution**:
1. Verify that the pkey specification is correct (1 for primary key fields)
2. Ensure that the id generation mechanism is creating unique identifiers
3. Check for duplicate records in the source data
4. Consider using composite primary keys if appropriate

### Dynamic Loading Problems

**Issue**: Custom mappings are not being loaded at runtime.

**Solution**:
1. Verify the file path is correct and accessible
2. Check that the file has the proper Prolog syntax
3. Ensure that the consult predicate is being called at the appropriate time
4. Verify that the user has permission to read the mapping file

### YAML Parsing Errors

**Issue**: YAML-based mappings are not being parsed correctly.

**Solution**:
1. Validate the YAML syntax using a YAML validator
2. Ensure proper indentation and formatting
3. Check for special characters that may need escaping
4. Verify that all required fields are present

### Performance Issues

**Issue**: Slow translation performance with complex mappings.

**Solution**:
1. Optimize field specifications to use appropriate data types and sizes
2. Minimize the number of attributes in frequently used entities
3. Consider indexing strategies for frequently queried fields
4. Use batch processing for large datasets

**Section sources**
- [mappings.pl](file://src/mappings.pl#L1-L518)
- [apiTranslations.pl](file://src/apiTranslations.pl#L1-L200)
- [gactoxml.pl](file://src/gactoxml.pl#L2048-L2316)

## Best Practices

This section outlines best practices for working with the data mapping system to ensure consistency, maintainability, and optimal performance.

### Consistent Naming Conventions

Adopt consistent naming conventions for:
- Mapping names: Use lowercase with underscores (e.g., "historical_act")
- Class names: Use camelCase (e.g., "HistoricalAct")
- Table names: Use plural lowercase with underscores (e.g., "historical_acts")
- Field names: Use lowercase with underscores (e.g., "creation_date")

### Documentation

Document all custom mappings with:
- Clear descriptions of the entity and its purpose
- Examples of source data that will use the mapping
- Notes on any special handling requirements
- Version information and change history

### Testing

Implement thorough testing for new mappings:
1. Create test cases with representative data
2. Verify that all fields are properly mapped
3. Test edge cases and boundary conditions
4. Validate data integrity after translation
5. Check performance with realistic data volumes

### Version Control

Use version control for mapping files:
- Track changes to mappings over time
- Maintain backward compatibility when possible
- Document breaking changes clearly
- Use branching for experimental mappings

### Performance Optimization

Optimize mappings for performance:
- Use appropriate data types and sizes
- Minimize the number of attributes in frequently used entities
- Consider indexing strategies for frequently queried fields
- Use batch processing for large datasets
- Monitor and optimize translation performance

### Error Handling

Implement robust error handling:
- Validate mapping syntax during development
- Provide clear error messages for mapping issues
- Implement fallback mechanisms for missing mappings
- Log mapping-related errors for troubleshooting

### Security

Follow security best practices:
- Validate user-provided mapping files
- Restrict access to mapping files based on user roles
- Sanitize input data to prevent injection attacks
- Regularly audit mapping configurations

### Collaboration

Facilitate collaboration:
- Share mapping definitions across research teams
- Establish review processes for new mappings
- Document mapping decisions and rationale
- Provide training on the mapping system

By following these best practices, researchers can ensure that their mapping configurations are reliable, maintainable, and optimized for their specific research needs.

**Section sources**
- [mappings.pl](file://src/mappings.pl#L1-L518)
- [person-mapping.yml](file://tests/kleio-home/mappings/person-mapping.yml#L1-L15)
- [sample-mapping.yml](file://tests/kleio-home/mappings/sample-mapping.yml#L1-L24)