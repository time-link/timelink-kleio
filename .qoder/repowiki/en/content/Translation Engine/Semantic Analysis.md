# Semantic Analysis

<cite>
**Referenced Files in This Document**
- [dataCDS.pl](file://src/dataCDS.pl)
- [dataCode.pl](file://src/dataCode.pl)
- [dataDictionary.pl](file://src/dataDictionary.pl)
- [struSyntax.pl](file://src/struSyntax.pl)
- [struCode.pl](file://src/struCode.pl)
- [externals.pl](file://src/externals.pl)
- [apiTranslations.pl](file://src/apiTranslations.pl)
- [errors.pl](file://src/errors.pl)
- [logging.pl](file://src/logging.pl)
</cite>

## Table of Contents
1. [Introduction](#introduction)
2. [Project Structure](#project-structure)
3. [Core Components](#core-components)
4. [Architecture Overview](#architecture-overview)
5. [Detailed Component Analysis](#detailed-component-analysis)
6. [Dependency Analysis](#dependency-analysis)
7. [Performance Considerations](#performance-considerations)
8. [Troubleshooting Guide](#troubleshooting-guide)
9. [Conclusion](#conclusion)

## Introduction
This document explains the semantic analysis phase of the translation engine. It focuses on how parsed structures are validated against schema definitions, how context-aware normalization rules are applied, and how semantic relationships between entities are established. It details Current Data Structure (CDS) management, group hierarchy processing, element validation against dictionary definitions, path resolution for nested groups, entity linking mechanisms, and constraint checking. Examples cover semantic validation rules, inheritance handling, and relationship establishment. Performance optimization strategies for complex hierarchical structures and debugging techniques for semantic errors are also provided.

## Project Structure
The semantic analysis spans several modules:
- Schema parsing and command execution: struSyntax.pl, struCode.pl
- Dictionary and hierarchy management: dataDictionary.pl
- Current Data Storage (CDS): dataCDS.pl
- Data processing and semantic checks: dataCode.pl
- External API to access current state and dictionary: externals.pl
- Translation orchestration and file-to-schema mapping: apiTranslations.pl
- Error reporting and logging: errors.pl, logging.pl

```mermaid
graph TB
subgraph "Schema Processing"
SS["struSyntax.pl"] --> SC["struCode.pl"]
SC --> DD["dataDictionary.pl"]
end
subgraph "Runtime Data"
DC["dataCode.pl"] --> CDS["dataCDS.pl"]
DC --> DD
DC --> EX["externals.pl"]
end
subgraph "Orchestration"
AT["apiTranslations.pl"] --> SS
AT --> DC
end
subgraph "Cross-cutting"
ERR["errors.pl"]
LOG["logging.pl"]
end
SS --- ERR
SC --- ERR
DC --- ERR
AT --- ERR
DC --- LOG
DD --- LOG
```

**Diagram sources**
- [struSyntax.pl:1-120](file://src/struSyntax.pl#L1-L120)
- [struCode.pl:1-120](file://src/struCode.pl#L1-L120)
- [dataDictionary.pl:1-120](file://src/dataDictionary.pl#L1-L120)
- [dataCode.pl:1-120](file://src/dataCode.pl#L1-L120)
- [dataCDS.pl:1-120](file://src/dataCDS.pl#L1-L120)
- [externals.pl:1-120](file://src/externals.pl#L1-L120)
- [apiTranslations.pl:1-120](file://src/apiTranslations.pl#L1-L120)
- [errors.pl:1-120](file://src/errors.pl#L1-L120)
- [logging.pl:1-120](file://src/logging.pl#L1-L120)

**Section sources**
- [struSyntax.pl:1-120](file://src/struSyntax.pl#L1-L120)
- [struCode.pl:1-120](file://src/struCode.pl#L1-L120)
- [dataDictionary.pl:1-120](file://src/dataDictionary.pl#L1-L120)
- [dataCode.pl:1-120](file://src/dataCode.pl#L1-L120)
- [dataCDS.pl:1-120](file://src/dataCDS.pl#L1-L120)
- [externals.pl:1-120](file://src/externals.pl#L1-L120)
- [apiTranslations.pl:1-120](file://src/apiTranslations.pl#L1-L120)
- [errors.pl:1-120](file://src/errors.pl#L1-L120)
- [logging.pl:1-120](file://src/logging.pl#L1-L120)

## Core Components
- Current Data Storage (CDS): Maintains the current group, path, elements, entries, and aspects during data processing. Provides getters/setters and helpers to build IDs and inspect ancestors.
- Data Dictionary: Stores schema definitions (groups, elements), containment relations, inheritance via fons/source, and default property propagation.
- Data Code: Orchestrates parsing callbacks (newGroup, newElement, endElement, storeCore), validates elements against the dictionary, updates CDS, constructs IDs, enforces constraints (e.g., certe), and flushes groups to storage.
- Syntax and Command Execution: Parses structure files, executes commands (nomino, pars, terminus), and populates the dictionary with defaults and inheritance.
- External API: Exposes current state and dictionary queries to exporters and semantic processors.
- Orchestration: Maps source files to structure files, initializes processing, and coordinates translation jobs.
- Errors and Logging: Centralized error/warning reporting with context; structured logging for diagnostics.

**Section sources**
- [dataCDS.pl:1-120](file://src/dataCDS.pl#L1-L120)
- [dataDictionary.pl:1-120](file://src/dataDictionary.pl#L1-L120)
- [dataCode.pl:1-120](file://src/dataCode.pl#L1-L120)
- [struSyntax.pl:1-120](file://src/struSyntax.pl#L1-L120)
- [struCode.pl:1-120](file://src/struCode.pl#L1-L120)
- [externals.pl:1-120](file://src/externals.pl#L1-L120)
- [apiTranslations.pl:1-120](file://src/apiTranslations.pl#L1-L120)
- [errors.pl:1-120](file://src/errors.pl#L1-L120)
- [logging.pl:1-120](file://src/logging.pl#L1-L120)

## Architecture Overview
The semantic analysis pipeline integrates schema loading, runtime data accumulation, and validation:

```mermaid
sequenceDiagram
participant API as "apiTranslations.pl"
participant SY as "struSyntax.pl"
participant SC as "struCode.pl"
participant DD as "dataDictionary.pl"
participant DC as "dataCode.pl"
participant CDS as "dataCDS.pl"
participant EX as "externals.pl"
participant ERR as "errors.pl"
participant LOG as "logging.pl"
API->>API : resolve structure per file
API->>SY : parse structure file
SY->>SC : compile_command(...)
SC->>DD : create_groups / set_defaults / copy_fons
SY-->>SC : execParam(...)
SC->>DD : set_group_prop / set_element_prop
API->>DC : initData()
loop For each group/element
DC->>CDS : newGroup()/initNewGroup()
DC->>DD : contained_by(), super_groups()
DC->>CDS : updatePath(), makeID()
DC->>EX : clio_element_extends(), clio_parts()
DC->>ERR : error_out(...) on violations
DC->>LOG : log_debug(...)
end
DC->>CDS : endElement()/storeElement()
DC->>CDS : flushGroup()/db_store()
```

**Diagram sources**
- [apiTranslations.pl:440-484](file://src/apiTranslations.pl#L440-L484)
- [struSyntax.pl:48-120](file://src/struSyntax.pl#L48-L120)
- [struCode.pl:105-120](file://src/struCode.pl#L105-L120)
- [dataDictionary.pl:118-147](file://src/dataDictionary.pl#L118-L147)
- [dataCode.pl:115-152](file://src/dataCode.pl#L115-L152)
- [dataCDS.pl:153-214](file://src/dataCDS.pl#L153-L214)
- [externals.pl:124-151](file://src/externals.pl#L124-L151)
- [errors.pl:85-113](file://src/errors.pl#L85-L113)
- [logging.pl:98-113](file://src/logging.pl#L98-L113)

## Detailed Component Analysis

### Current Data Structure (CDS) Management
- Purpose: Holds the current group, its ID, ancestor path, element list, current element, entry lists (core/original/comment), and aspect pointer.
- Key operations:
  - Create/clean/reset: initialize empty state and counters.
  - Get/Set fields: typed accessors for all fields.
  - Aspect extraction: retrieve core/original/comment values, supporting multiple entries.
  - Ancestor and ID utilities: compute group ID from identification element or counter.
- Complexity: Most operations are O(1) except list scans for elements and aspects.

```mermaid
classDiagram
class CDS {
+cpath : list
+cgroup : atom
+cgroupID : string
+locusCount : nonneg
+elementList : list
+celement : atom
+entryList : list
+coreEntryList : list
+originalEntryList : list
+commentEntryList : list
+caspect : atom
+ccore : list
+coriginal : list
+ccomment : list
+getCDS(C)
+setCDS(C)
+getCDField(F,V)
+setCDField(F,V)
+makeID(ID)
+getCDAnc(A,ID)
+get_aspect(A,E,I)
}
```

**Diagram sources**
- [dataCDS.pl:91-104](file://src/dataCDS.pl#L91-L104)
- [dataCDS.pl:153-214](file://src/dataCDS.pl#L153-L214)
- [dataCDS.pl:307-328](file://src/dataCDS.pl#L307-L328)
- [dataCDS.pl:368-405](file://src/dataCDS.pl#L368-L405)
- [dataCDS.pl:450-498](file://src/dataCDS.pl#L450-L498)

**Section sources**
- [dataCDS.pl:153-214](file://src/dataCDS.pl#L153-L214)
- [dataCDS.pl:307-328](file://src/dataCDS.pl#L307-L328)
- [dataCDS.pl:368-405](file://src/dataCDS.pl#L368-L405)
- [dataCDS.pl:450-498](file://src/dataCDS.pl#L450-L498)

### Group Hierarchy Processing and Path Resolution
- Containment and inheritance:
  - Direct containment via pars/semper/solum/repetitio properties.
  - Super-group traversal via fons/source; cached results avoid repeated computation.
- Path update algorithm:
  - Prefer direct containment with current group.
  - If not found, search ancestors in reverse path order.
  - Support base-class matching for flexible linkage.
  - Detect recursion to prevent infinite loops.
- ID generation:
  - Use identification element if present and single-valued.
  - Otherwise use signum prefix plus incrementing counter.

```mermaid
flowchart TD
Start(["Enter updatePath"]) --> CheckDoc{"Is NewGroup a document?"}
CheckDoc --> |Yes| DocPath["Set path to [Doc=OldID]"] --> End(["Return NewPath"])
CheckDoc --> |No| TryDirect["contained_by(NewGroup, OldGroup)?"]
TryDirect --> |Yes| AppendPath["Append OldGroup to Path"] --> RecCheck["check_for_recursion(NewGroup, NewPath)"] --> End
TryDirect --> |No| ReversePath["Reverse Path and scan ancestors"]
ReversePath --> FoundAnc{"Found ancestor A s.t. contained_by(NewGroup, A)?"}
FoundAnc --> |Yes| CutPath["Cut path after A"] --> RecCheck --> End
FoundAnc --> |No| BaseClassMatch["Match by base classes?"]
BaseClassMatch --> |Yes| CutPath --> RecCheck --> End
BaseClassMatch --> |No| Fail["Error: cannot link group"] --> End
```

**Diagram sources**
- [dataCode.pl:217-281](file://src/dataCode.pl#L217-L281)
- [dataDictionary.pl:184-261](file://src/dataDictionary.pl#L184-L261)

**Section sources**
- [dataCode.pl:217-281](file://src/dataCode.pl#L217-L281)
- [dataDictionary.pl:184-261](file://src/dataDictionary.pl#L184-L261)

### Element Validation Against Dictionary Definitions
- Element existence:
  - Must be declared in the group’s locus/ceteri/certe lists.
  - Specialization allowed: an element extending a base element is accepted if the base is valid in the group.
- Implicit naming:
  - If no explicit element name is given, the next position in the group’s locus list is used.
- Constraint enforcement:
  - At group end, check that all certe elements are present; otherwise report missing elements.

```mermaid
flowchart TD
S(["endElement"]) --> E1["endEntry()"]
E1 --> N1["check_ename()"]
N1 --> V1["verify_element(E)"]
V1 --> D1{"element_of(E,G)?"}
D1 --> |Yes| OK["Proceed"]
D1 --> |No| EXT{"clio_element_extends(E,S) and element_of(S,G)?"}
EXT --> |Yes| OK
EXT --> |No| ERR["error_out('unknown element')"]
OK --> ST["storeElement()"]
ST --> FL["flushGroup() -> makeID(), check_elements(), db_store()"]
```

**Diagram sources**
- [dataCode.pl:340-404](file://src/dataCode.pl#L340-L404)
- [dataCode.pl:308-321](file://src/dataCode.pl#L308-L321)
- [dataCode.pl:154-168](file://src/dataCode.pl#L154-L168)
- [dataDictionary.pl:289-308](file://src/dataDictionary.pl#L289-L308)
- [externals.pl:140-151](file://src/externals.pl#L140-L151)

**Section sources**
- [dataCode.pl:340-404](file://src/dataCode.pl#L340-L404)
- [dataCode.pl:308-321](file://src/dataCode.pl#L308-L321)
- [dataCode.pl:154-168](file://src/dataCode.pl#L154-L168)
- [dataDictionary.pl:289-308](file://src/dataDictionary.pl#L289-L308)
- [externals.pl:140-151](file://src/externals.pl#L140-L151)

### Context-Aware Normalization Rules
- Aspects:
  - Core, original, comment aspects are tracked separately; multi-entry support wraps lists in a mult structure when needed.
- Leading/trailing whitespace:
  - Entry text is trimmed before finalizing entries.
- Locus-driven ordering:
  - When implicit element names are used, the locus count ensures correct positional assignment.

```mermaid
flowchart TD
A(["storeCore(I)"]) --> G["Get caspect"]
G --> C{"Aspect = core/original/comment?"}
C --> |core| P1["Prepend I to ccore"]
C --> |original| P2["Prepend I to coriginal"]
C --> |comment| P3["Prepend I to ccomment"]
P1 --> R(["Return"])
P2 --> R
P3 --> R
```

**Diagram sources**
- [dataCode.pl:475-488](file://src/dataCode.pl#L475-L488)
- [dataCDS.pl:390-405](file://src/dataCDS.pl#L390-L405)

**Section sources**
- [dataCode.pl:475-488](file://src/dataCode.pl#L475-L488)
- [dataCDS.pl:390-405](file://src/dataCDS.pl#L390-L405)

### Entity Linking Mechanisms and Relationship Establishment
- Same-as links:
  - Exporters can detect specializations of same_as elements and generate relation records linking current group to referenced external IDs.
- Base element lookup:
  - The external API provides clio_belement_aspect to find expected semantics even when source uses specialized element names.

```mermaid
sequenceDiagram
participant DC as "dataCode.pl"
participant EX as "externals.pl"
participant EXP as "Exporter (e.g., gactoxml)"
DC->>EX : clio_path(P)
DC->>EX : clio_belement_aspect(core,same_as,Content)
EXP->>EXP : normalize id, ensure relation class
EXP->>EXP : assert relation cache (same_as_cached)
EXP-->>DC : continue processing
```

**Diagram sources**
- [externals.pl:184-197](file://src/externals.pl#L184-L197)
- [gactoxml.pl:849-869](file://src/gactoxml.pl#L849-L869)

**Section sources**
- [externals.pl:184-197](file://src/externals.pl#L184-L197)
- [gactoxml.pl:849-869](file://src/gactoxml.pl#L849-L869)

### Inheritance Handling (Groups and Elements)
- Groups:
  - fons/source defines superclass; copy_fons_g propagates properties from source group to current group.
  - Generic groups allow prefix/suffix-based property inheritance (not commonly used).
- Elements:
  - fons/source allows element specialization; copy_fons_e copies properties from source element.
- Topological ordering:
  - Utility to traverse and sort hierarchies for consistent processing.

```mermaid
classDiagram
class Group {
+name : atom
+id : symbol
+fons : atom
+pars/semper/solum/repetitio : list
+signum : string
}
class Element {
+name : atom
+id : symbol
+fons : atom
+identificatio : sic/non
}
Group <.. Group : "extends via fons"
Element <.. Element : "extends via fons"
```

**Diagram sources**
- [struCode.pl:218-226](file://src/struCode.pl#L218-L226)
- [dataDictionary.pl:624-645](file://src/dataDictionary.pl#L624-L645)
- [dataDictionary.pl:665-686](file://src/dataDictionary.pl#L665-L686)

**Section sources**
- [struCode.pl:218-226](file://src/struCode.pl#L218-L226)
- [dataDictionary.pl:624-645](file://src/dataDictionary.pl#L624-L645)
- [dataDictionary.pl:665-686](file://src/dataDictionary.pl#L665-L686)

### Constraint Checking and Semantic Validation Rules
- Required elements (certe):
  - Enforced at group closure; missing elements produce detailed errors.
- Identification uniqueness:
  - Identification elements must be single-valued; multiple entries cause errors.
- Recursion prevention:
  - Path update detects cycles and aborts invalid nesting.

```mermaid
flowchart TD
F(["flushGroup"]) --> M["makeID()"]
M --> C["check_elements(G,GID)"]
C --> R{"All certe present?"}
R --> |No| E["error_out('missing element(s)')"]
R --> |Yes| D["db_store()"]
```

**Diagram sources**
- [dataCode.pl:140-168](file://src/dataCode.pl#L140-L168)
- [dataCode.pl:275-281](file://src/dataCode.pl#L275-L281)
- [dataCDS.pl:481-488](file://src/dataCDS.pl#L481-L488)

**Section sources**
- [dataCode.pl:140-168](file://src/dataCode.pl#L140-L168)
- [dataCode.pl:275-281](file://src/dataCode.pl#L275-L281)
- [dataCDS.pl:481-488](file://src/dataCDS.pl#L481-L488)

### Path Resolution Algorithm for Nested Groups
- Decision points:
  - Is the new group a document?
  - Is it directly contained by the previous group?
  - Does it match any ancestor in the current path?
  - Does it match by base class?
- Outcome:
  - Construct longest possible path without cycles.

```mermaid
flowchart TD
U(["updatePath(OldGroup,OldID,NewGroup,Path)"]) --> D1{"isDoc(NewGroup)?"}
D1 --> |Yes| Z["Path=[]"] --> X(["Return []"])
D1 --> |No| T1{"contained_by(NewGroup, OldGroup)?"}
T1 --> |Yes| A1["Append [OldGroup=OldID] to Path"] --> R1["check_for_recursion"] --> X
T1 --> |No| B1["Scan reversed Path for ancestor A"]
B1 --> F1{"contained_by(NewGroup, A)?"}
F1 --> |Yes| C1["Cut path after A"] --> R1 --> X
F1 --> |No| B2["Base class match?"]
B2 --> |Yes| C1 --> R1 --> X
B2 --> |No| Err["Error: cannot link"] --> X
```

**Diagram sources**
- [dataCode.pl:217-281](file://src/dataCode.pl#L217-L281)
- [dataDictionary.pl:184-261](file://src/dataDictionary.pl#L184-L261)

**Section sources**
- [dataCode.pl:217-281](file://src/dataCode.pl#L217-L281)
- [dataDictionary.pl:184-261](file://src/dataDictionary.pl#L184-L261)

## Dependency Analysis
Key dependencies among semantic components:

```mermaid
graph LR
SY["struSyntax.pl"] --> SC["struCode.pl"]
SC --> DD["dataDictionary.pl"]
DC["dataCode.pl"] --> CDS["dataCDS.pl"]
DC --> DD
DC --> EX["externals.pl"]
AT["apiTranslations.pl"] --> SY
AT --> DC
ERR["errors.pl"] --> SY
ERR --> SC
ERR --> DC
LOG["logging.pl"] --> DC
LOG --> DD
```

**Diagram sources**
- [struSyntax.pl:1-120](file://src/struSyntax.pl#L1-L120)
- [struCode.pl:1-120](file://src/struCode.pl#L1-L120)
- [dataDictionary.pl:1-120](file://src/dataDictionary.pl#L1-L120)
- [dataCode.pl:1-120](file://src/dataCode.pl#L1-L120)
- [dataCDS.pl:1-120](file://src/dataCDS.pl#L1-L120)
- [externals.pl:1-120](file://src/externals.pl#L1-L120)
- [apiTranslations.pl:1-120](file://src/apiTranslations.pl#L1-L120)
- [errors.pl:1-120](file://src/errors.pl#L1-L120)
- [logging.pl:1-120](file://src/logging.pl#L1-L120)

**Section sources**
- [struSyntax.pl:1-120](file://src/struSyntax.pl#L1-L120)
- [struCode.pl:1-120](file://src/struCode.pl#L1-L120)
- [dataDictionary.pl:1-120](file://src/dataDictionary.pl#L1-L120)
- [dataCode.pl:1-120](file://src/dataCode.pl#L1-L120)
- [dataCDS.pl:1-120](file://src/dataCDS.pl#L1-L120)
- [externals.pl:1-120](file://src/externals.pl#L1-L120)
- [apiTranslations.pl:1-120](file://src/apiTranslations.pl#L1-L120)
- [errors.pl:1-120](file://src/errors.pl#L1-L120)
- [logging.pl:1-120](file://src/logging.pl#L1-L120)

## Performance Considerations
- Containment caching:
  - Cache positive and negative containment results to reduce repeated graph traversals.
- Record-based CDS:
  - Use record variants for faster field access and reduced term copying.
- Efficient list operations:
  - Prepend entries to lists and reverse once at endEntry to minimize overhead.
- Mutexed initialization:
  - Protect structure and data initialization with mutexes to avoid redundant work in concurrent environments.
- Logging level control:
  - Adjust log levels to reduce overhead in production runs.

[No sources needed since this section provides general guidance]

## Troubleshooting Guide
- Common semantic errors:
  - Unknown element in group: verify element declaration or inheritance.
  - Missing required elements (certe): ensure all required fields are present before group closure.
  - Multiple entries in identification element: ensure single-valued identification.
  - Recursive group nesting: fix hierarchy to avoid cycles.
- Debugging techniques:
  - Enable debug logging to trace path updates and containment checks.
  - Inspect CDS state using showCDS-like routines to visualize current group and entries.
  - Review error reports with line context for precise localization.

**Section sources**
- [dataCode.pl:308-321](file://src/dataCode.pl#L308-L321)
- [dataCode.pl:154-168](file://src/dataCode.pl#L154-L168)
- [dataCDS.pl:506-516](file://src/dataCDS.pl#L506-L516)
- [errors.pl:85-113](file://src/errors.pl#L85-L113)
- [logging.pl:98-113](file://src/logging.pl#L98-L113)

## Conclusion
The semantic analysis phase integrates schema-driven validation, context-aware normalization, and robust hierarchy management. Through careful CDS design, efficient containment checks, and clear error reporting, the system supports complex hierarchical structures while maintaining performance and debuggability. Proper use of inheritance, identification rules, and relationship establishment enables rich semantic modeling across diverse source documents.