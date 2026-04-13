# Kleio Inference Rules - YAML Format Reference

Kleio's inference engine automatically generates relationships and attributes
from the hierarchical structure of parsed `.cli` files. Rules are defined in
YAML files and matched against the group hierarchy during post-processing.

This document describes how to write custom inference rules.

## Overview

Inference rules follow a **when/then** pattern:

- **when**: A sequence of conditions that match against the group hierarchy path
- **then**: A list of actions to execute when all conditions match

The engine walks every path from the document root to each leaf group in the
parsed data. For each path it tries every rule. When all conditions in a rule
match consecutive groups in the path, the rule fires and its actions are
executed.

## File Structure

A rules file is a YAML document with a single top-level key `rules` containing
a list of rule definitions:

```yaml
rules:
  - name: rule_name
    description: "Human-readable description"
    priority: 0
    when:
      # list of conditions
    then:
      # list of actions
```

### Rule Fields

| Field | Required | Type | Description |
|-------|----------|------|-------------|
| `name` | yes | string | Unique identifier for the rule |
| `description` | no | string | Human-readable explanation |
| `priority` | no | integer | Execution order (higher = later). Default: `0` |
| `when` | yes | list | Conditions to match |
| `then` | yes | list | Actions to execute on match |

## Conditions

Conditions are matched in order against consecutive groups in a hierarchy path.
Each condition is a single-key dictionary.

### `sequence` - Match Any Ancestor Sequence

Matches zero or more groups in the path (like a wildcard). Almost every rule
starts with `sequence: any` so the pattern can appear at any depth.

```yaml
- sequence: any
```

### `group` - Match a Group by Name

Matches a group with an exact name. Optionally binds the group's ID to a
variable for use in actions.

```yaml
# Full form
- group: {name: pai, bind: father_id}

# Shorthand (no binding)
- group: pai
```

| Parameter | Required | Description |
|-----------|----------|-------------|
| `name` | yes | Exact group name to match |
| `bind` | no | Variable name to capture the matched group's ID |

### `extends` - Match a Group by Base Class

Matches any group whose base class (via the schema inheritance chain) equals
the specified name. This is more flexible than `group` because it matches all
specializations.

```yaml
# Full form
- extends: {base: actorm, bind: child_id}

# Shorthand (no binding)
- extends: person
```

| Parameter | Required | Description |
|-----------|----------|-------------|
| `base` | yes | Base class name to match against |
| `bind` | no | Variable name to capture the matched group's ID |

### `element` - Extract an Element Value

Matches a group that contains a specific element and optionally binds the
element's core value to a variable.

```yaml
# Full form
- element: {name: sexo, bind: gender}

# Shorthand (no binding)
- element: sexo
```

| Parameter | Required | Description |
|-----------|----------|-------------|
| `name` | yes | Element name to look for in the current group |
| `bind` | no | Variable name to capture the element's core text value |

### `clause` - Custom Predicate

Reserved for programmatic rules defined in Python. Allows arbitrary callable
predicates as conditions.

```yaml
- clause: custom
```

In practice, clause-based conditions are registered via the Python API rather
than YAML.

## Actions

Actions are executed when all conditions match. Variable names used in actions
must have been bound by a condition's `bind` parameter.

### `relation` - Generate a Relationship

Creates a directed relationship between two entities identified by their bound
variables.

```yaml
- relation:
    type: parentesco       # relationship category
    value: pai             # specific relationship value
    origin: father_id      # variable holding origin entity ID
    dest: child_id         # variable holding destination entity ID
```

| Parameter | Required | Description |
|-----------|----------|-------------|
| `type` | yes | Relationship category (e.g., `parentesco`) |
| `value` | yes | Specific relationship (e.g., `pai`, `mae`, `marido`) |
| `origin` | yes | Variable name bound to the origin entity's ID |
| `dest` | yes | Variable name bound to the destination entity's ID |

### `attribute` - Generate an Attribute

Assigns a typed attribute to an entity.

```yaml
- attribute:
    entity: father_id      # variable holding the entity ID
    type: ec               # attribute type (e.g., marital status)
    value: c               # attribute value (literal)
```

To use a bound variable as the value instead of a literal string, add
`value_is_var: true`:

```yaml
- attribute:
    entity: person_id
    type: gender
    value: gender_var      # this is now treated as a variable name
    value_is_var: true
```

| Parameter | Required | Description |
|-----------|----------|-------------|
| `entity` | yes | Variable name bound to the target entity's ID |
| `type` | yes | Attribute type name |
| `value` | yes | Literal value or variable name (see `value_is_var`) |
| `value_is_var` | no | If `true`, `value` is resolved as a variable. Default: `false` |

### `newscope` - Reset Matching Context

Resets the inference engine's internal scope. Use this at structural boundaries
(e.g., when entering a new historical act) to prevent variable bindings from
leaking across unrelated records.

```yaml
- newscope: true
```

## Complete Examples

### Father Relationship

When a male actor (`actorm`) has a sub-group named `pai` (father), generate
a "father" relationship:

```yaml
- name: father_of_male_actor
  description: "Generate father relationship from pai to male actor"
  when:
    - sequence: any
    - extends: {base: actorm, bind: child_id}
    - group: {name: pai, bind: father_id}
  then:
    - relation: {type: parentesco, value: pai, origin: father_id, dest: child_id}
```

### Marriage with Marital Status Attributes

When a male actor has a sub-group `mulher` (wife), generate the marriage
relationship and set the marital status attribute on both:

```yaml
- name: male_actor_with_wife
  description: "Generate husband relation from male actor to wife"
  when:
    - sequence: any
    - extends: {base: actorm, bind: husband_id}
    - group: {name: mulher, bind: wife_id}
  then:
    - relation: {type: parentesco, value: marido, origin: husband_id, dest: wife_id}
    - attribute: {entity: husband_id, type: ec, value: c}
    - attribute: {entity: wife_id, type: ec, value: c}
```

### Grandparent (Paternal Grandfather)

When a `pai` group contains a `ppai` sub-group, the ppai is the father of the
pai:

```yaml
- name: paternal_grandfather
  description: "Generate father relation from paternal grandfather (ppai) to father"
  when:
    - sequence: any
    - group: {name: pai, bind: son_id}
    - group: {name: ppai, bind: parent_id}
  then:
    - relation: {type: parentesco, value: pai, origin: parent_id, dest: son_id}
```

### Previous Marriage (Widower)

Track previous spouses and mark them as deceased:

```yaml
- name: previous_wife_1
  description: "Previous wife (mulher1) of a male actor"
  when:
    - sequence: any
    - extends: {base: actorm, bind: husband_id}
    - group: {name: mulher1, bind: wife_id}
  then:
    - relation: {type: parentesco, value: foi-marido, origin: husband_id, dest: wife_id}
    - attribute: {entity: wife_id, type: morta, value: antes}
```

### Scope Reset at Act Boundaries

Prevent bindings from one act from affecting the next:

```yaml
- name: new_scope_historical_act
  description: "Reset scope when entering a historical act"
  priority: 100
  when:
    - sequence: any
    - extends: {base: historical-act}
  then:
    - newscope: true
```

## Loading Custom Rules

### From the API / Configuration

Place your YAML rules file in the `inferences/` directory under `KLEIO_HOME_DIR`.
The engine loads the default rules first, then any user-defined files.

### From Python

```python
from kleio.inference.engine import InferenceEngine

engine = InferenceEngine()
engine.load_rules_from_yaml("path/to/my_rules.yaml")
```

Rules are evaluated in registration order, modified by `priority`. Lower
priority values run first.

## Matching Semantics

1. The engine extracts all root-to-leaf paths from the parsed group tree.
2. For each path, it tries every registered rule.
3. A rule's conditions are matched left-to-right against consecutive groups in
   the path. A `sequence` condition can skip over zero or more groups.
4. When all conditions match, variable bindings are collected and actions are
   executed with those bindings.
5. Duplicate relations and attributes (same type + entities) are automatically
   suppressed.

## Tips for Writing Rules

- Always start with `sequence: any` unless the rule must match from the
  document root.
- Use `extends` for broad matching (all persons, all actors) and `group` for
  exact name matching.
- Keep variable names descriptive (`father_id` not `x`).
- Use `priority` to control ordering when rules depend on each other. Scope
  resets should have high priority values so they run after relationship rules.
- Test rules against known data before deploying to production.
