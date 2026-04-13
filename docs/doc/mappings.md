# Mappings YAML Format

Mappings connect Kleio source-oriented groups to entities in the person-oriented data model used by Timelink. They define how groups in transcribed historical documents are stored in the relational database.

Mapping files are placed in the `mappings/` directory under `KLEIO_HOME_DIR` and are loaded automatically during translation.

## Overview

A mapping file contains two types of entries:

1. **Mapping entries** — associate a Kleio group name with a database class.
2. **Class definitions** — describe the database table, inheritance, and column layout for a class.

Both entry types coexist in the same YAML file as a top-level list.

## File format

Mapping files use the `.yml` or `.yaml` extension. The top-level structure is a YAML list where each item is either a `mapping` or a `class` entry.

```yaml
# my-mapping.yml
- mapping: { name: minutes, class: minutes }

- class:
    name: minutes
    extends: act
    table: minutes
    description: >
      This class represents the minutes of a meeting.
      It is a subclass of the class act.
    attributes:
      - { name: id, column: id, class: id, type: string, size: 64, pkey: true }
      - { name: the_day, column: the_day, type: number, size: 2, class: day }
      - { name: the_month, column: the_month, type: number, class: month }
      - { name: the_year, column: the_year, type: number, class: year }
      - { name: summary, type: string, class: text, size: 32768 }
      - { name: obs, type: text, class: text, size: 32768 }

- mapping: { name: acta, class: minutes }
```

## Mapping entries

A mapping entry associates a Kleio group name with a target class. Multiple groups can map to the same class.

```yaml
- mapping: { name: <group_name>, class: <class_name> }
```

| Field   | Required | Description                                          |
|---------|----------|------------------------------------------------------|
| `name`  | Yes      | The Kleio group name as it appears in `.cli` files.  |
| `class` | Yes      | The target class name for database storage.          |

### Examples

```yaml
# Single group to class
- mapping: { name: person, class: person }

# Multiple groups sharing the same class
- mapping: { name: acta, class: minutes }
- mapping: { name: amz, class: minutes }

# Group name with hyphens (quote if needed)
- mapping: { name: authority-register, class: aregister }
```

## Class definitions

A class definition describes the database table structure for a mapped class: its parent class, table name, and the list of column attributes.

```yaml
- class:
    name: <class_name>
    extends: <parent_class>
    table: <table_name>
    description: <optional description>
    attributes:
      - { name: <attr>, column: <col>, class: <base>, type: <type>, size: <n>, precision: <p>, pkey: <bool> }
      ...
```

### Class-level fields

| Field         | Required | Description                                                 |
|---------------|----------|-------------------------------------------------------------|
| `name`        | Yes      | Unique class identifier, referenced in `mapping` entries.   |
| `extends`     | Yes      | Parent class (inheritance). Common roots: `entity`, `act`, `object`, `person`. |
| `table`       | Yes      | Database table name where instances are stored.             |
| `description` | No       | Human-readable description of the class purpose.            |
| `attributes`  | Yes      | List of attribute (column) definitions.                     |

### Attribute fields

Each attribute in the `attributes` list defines a database column.

| Field       | Required | Default   | Description                                         |
|-------------|----------|-----------|-----------------------------------------------------|
| `name`      | Yes      | —         | Attribute name (matches a Kleio element name).      |
| `column`    | No       | same as `name` | Database column name. Use when the column name differs from the attribute name (e.g., `the_type` for a `type` attribute to avoid SQL reserved words). |
| `class`     | No       | same as `name` | Base class of this attribute. Used for inheritance resolution; maps to the canonical attribute name in the parent entity model. |
| `type`      | Yes      | —         | Column data type. See table below.                  |
| `size`      | No       | varies    | Maximum column size (length for strings, digits for numbers). |
| `precision` | No       | `0`       | Decimal precision (for numeric types only).         |
| `pkey`      | No       | `false`   | Whether this column is part of the primary key. Accepts `true`/`false`, `1`/`0`, or `yes`/`no`. |

### Supported data types

| Type     | Description                        | Typical `size` |
|----------|------------------------------------|----------------|
| `string` / `varchar` | Variable-length character string | 64–1024     |
| `text`   | Long text field                    | 16384–32768    |
| `number` / `numeric` / `int` | Numeric value          | 2–12           |
| `char`   | Fixed-length character             | 1              |

## Class inheritance hierarchy

Classes form an inheritance tree. The `extends` field defines the parent. The base classes provided by the system are:

```
entity
├── person
├── object
│   └── good
├── act
├── source
├── relation
├── attribute
└── geoentity
```

When a class extends another, it inherits the parent's attributes. The `class` field in each attribute maps back to the canonical attribute name in the base model, enabling the system to recognize inherited attributes across different table layouts.

## Loading and directory structure

Mapping files are loaded from the `mappings/` directory:

```
KLEIO_HOME_DIR/
└── mappings/
    ├── sample-mapping.yml
    ├── person-mapping.yml
    └── geodesc-mapping.yml
```

All `.yml` and `.yaml` files in this directory are loaded automatically. Files are processed in alphabetical order. JSON mapping files (`.json`) with simple key-value pairs are also supported for basic value normalization.

### Loading priority

- YAML and JSON files are loaded; Prolog `.pl` files are skipped with a debug message.
- If multiple files define the same class name, the last-loaded definition takes precedence.
- If multiple files map the same group name, the last-loaded mapping wins.

## Simple value mappings

In addition to group-to-class mappings, a mapping file can be a simple YAML dictionary for data normalization (e.g., abbreviation expansion or vocabulary standardization):

```yaml
# abbreviations.yml
m: masculino
f: feminino
n/a: desconhecido
```

These are loaded by file stem name (e.g., `abbreviations`) and applied during export with:

```python
store.apply("m", "abbreviations")  # returns "masculino"
```

## Correspondence with Prolog format

The YAML format replaces the legacy Prolog operator-based syntax. Here is the correspondence:

### Prolog (legacy)

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
        obs column obs baseclass obs coltype varchar colsize 16654 colprecision 0 pkey 0 .
```

### YAML (current)

```yaml
- mapping: { name: person, class: person }
- class:
    name: person
    extends: entity
    table: persons
    attributes:
      - { name: id, column: id, class: id, type: varchar, size: 64, pkey: true }
      - { name: name, column: name, class: name, type: varchar, size: 128 }
      - { name: sex, column: sex, class: sex, type: char, size: 1 }
      - { name: obs, column: obs, class: obs, type: varchar, size: 16654 }
```

### Keyword mapping

| Prolog keyword | YAML field    |
|---------------|---------------|
| `super`       | `extends`     |
| `table`       | `table`       |
| `baseclass`   | `class`       |
| `column`      | `column`      |
| `coltype`     | `type`        |
| `colsize`     | `size`        |
| `colprecision`| `precision`   |
| `pkey`        | `pkey`        |

## Complete example

Below is a full mapping file for a historical notarial documents project:

```yaml
# notarial-mapping.yml

# Map group names to classes
- mapping: { name: escritura, class: escritura }
- mapping: { name: bem, class: good }
- mapping: { name: fogo, class: household }

# Class: escritura (notarial deed)
- class:
    name: escritura
    extends: act
    table: escrituras
    description: Notarial deeds transcribed from historical archives.
    attributes:
      - { name: id, column: id, class: id, type: string, size: 64, pkey: true }
      - { name: date, column: the_date, class: date, type: string, size: 24 }
      - { name: type, column: the_type, class: type, type: string, size: 32 }
      - { name: loc, column: loc, class: loc, type: string, size: 64 }
      - { name: fol, column: fol, class: fol, type: string, size: 64 }
      - { name: sumario, column: summary, class: summary, type: string, size: 1024 }
      - { name: obs, column: obs, class: obs, type: varchar, size: 16654 }

# Class: household (census household)
- class:
    name: household
    extends: object
    table: households
    attributes:
      - { name: id, column: id, class: id, type: string, size: 64, pkey: true }
      - { name: dia, column: the_day, class: day, type: number, size: 2 }
      - { name: mes, column: the_month, class: month, type: number, size: 2 }
      - { name: ano, column: the_year, class: year, type: number, size: 4 }
      - { name: loc, column: loc, class: loc, type: string, size: 64 }
      - { name: obs, column: obs, class: obs, type: varchar, size: 16654 }
```

## See also

- [Inference Rules YAML Format](inference_rules.md) — declarative rules for automatic relationship and attribute generation.
- [Translation Results](translation_results.md) — description of the output produced by the translation pipeline.
