# Kleio Notation

<cite>
**Referenced Files in This Document**   
- [dataSyntax.pl](file://src/dataSyntax.pl)
- [lexical.pl](file://src/lexical.pl)
- [clioPP.pl](file://src/clioPP.pl)
- [struSyntax.pl](file://src/struSyntax.pl)
- [struCode.pl](file://src/struCode.pl)
- [notarial80.cli](file://tests/kleio-home/sources/reference_sources/notariais/notarial80.cli)
- [dehergne-a.cli](file://tests/kleio-home/sources/reference_sources/linked_data/dehergne-a.cli)
- [Devedores.cli](file://tests/kleio-home/sources/reference_sources/varia/Devedores.cli)
- [_sources-structure.yaml](file://tests/kleio-home/structures/_sources-structure.yaml)
</cite>

## Table of Contents
1. [Introduction](#introduction)
2. [Kleio File Structure](#kleio-file-structure)
3. [Element Declarations and Part Hierarchies](#element-declarations-and-part-hierarchies)
4. [Metadata Annotations](#metadata-annotations)
5. [Domain-Specific Language for Historical Transcription](#domain-specific-language-for-historical-transcription)
6. [Common Patterns in Historical Documents](#common-patterns-in-historical-documents)
7. [Relationship to Data Model](#relationship-to-data-model)
8. [Writing Valid Kleio Syntax](#writing-valid-kleio-syntax)
9. [Processing Special Markers](#processing-special-markers)
10. [Conclusion](#conclusion)

## Introduction
Kleio notation is a domain-specific language designed for the structured transcription of unstructured historical sources. It provides a systematic approach to representing historical document transcriptions through a formal syntax that enables precise encoding of complex relationships, entities, and metadata. The Kleio system uses .cli files as its primary format for storing transcribed historical data, with a syntax that supports hierarchical organization, metadata annotation, and linked data integration. This notation system transforms free-form historical records into structured data that can be processed, analyzed, and linked across different sources.

**Section sources**
- [dataSyntax.pl](file://src/dataSyntax.pl#L1-L194)
- [lexical.pl](file://src/lexical.pl#L1-L492)

## Kleio File Structure
Kleio (.cli) files follow a hierarchical structure that begins with a top-level "kleio" group declaration, which serves as the root container for all content within the file. The structure follows a parent-child relationship where groups can contain other groups, elements, and attributes. Each file typically starts with metadata about the source document, followed by the actual transcribed content organized into logical units.

The syntax uses a simple but powerful format where each line represents either a group declaration, an element, or an attribute. Group declarations follow the pattern `groupname$identifier`, where the dollar sign ($) separates the group type from its unique identifier. Indentation is used to represent the hierarchical relationship between elements, with deeper indentation indicating nested content.

For example, in a notarial document transcription, the structure might begin with a "kleio" group that contains a "fonte" (source) group, which in turn contains one or more "escritura" (deed) groups, each with their own nested participants, properties, and attributes. This hierarchical approach allows for the representation of complex document structures while maintaining readability and organization.

**Section sources**
- [dataSyntax.pl](file://src/dataSyntax.pl#L50-L63)
- [notarial80.cli](file://tests/kleio-home/sources/reference_sources/notariais/notarial80.cli#L1-L800)

## Element Declarations and Part Hierarchies
Element declarations in Kleio notation define the building blocks of the transcription system. Elements are declared using the syntax `elementname$identifier`, where the element name specifies the type of entity being represented and the identifier provides a unique reference. Elements can be simple data fields or complex containers that hold other elements and attributes.

The part hierarchy system in Kleio defines how different elements relate to each other within the document structure. This is specified in the underlying data model (typically defined in YAML structure files) and determines which elements can be contained within others. For instance, a "person" element might be allowed as a part of a "historical-act" group, indicating that persons can participate in historical events documented in the source.

Elements can have various properties defined in the structure, including whether they are required (guaranteed), optional (also), or can appear multiple times (arbitrary). The position parameter determines the order in which elements should appear within their parent group, ensuring consistency across transcriptions. This hierarchical and constrained approach ensures that transcriptions adhere to a consistent schema while allowing flexibility for different types of historical documents.

**Section sources**
- [_sources-structure.yaml](file://tests/kleio-home/structures/_sources-structure.yaml#L1-L800)
- [struSyntax.pl](file://src/struSyntax.pl#L1-L417)

## Metadata Annotations
Metadata annotations in Kleio notation provide contextual information about the transcription and its source. These annotations are typically included as elements within the top-level "kleio" group and include information such as the source document identifier, date, location, and additional observations. The "fonte" element is used to declare the source of the transcription, with attributes specifying the document type, date range, physical location, and any relevant observations.

Special metadata elements include "link" declarations that establish connections to external linked data sources, such as Wikidata or library catalogs. These links use a template format where placeholders (like $1) are replaced with actual identifiers during processing. For example, a link to Wikidata might be declared as `link$wikidata/"https://www.wikidata.org/wiki/$1"`, allowing individual entities in the transcription to be annotated with their corresponding Wikidata identifiers.

Comments and original wording are preserved using special markers: the percent sign (%) for original wording from the source document, and the hash sign (#) for transcriber comments or annotations. These annotations are crucial for maintaining the integrity of the transcription process, allowing researchers to distinguish between the original text and interpretive additions by the transcriber.

**Section sources**
- [dehergne-a.cli](file://tests/kleio-home/sources/reference_sources/linked_data/dehergne-a.cli#L1-L800)
- [clioPP.pl](file://src/clioPP.pl#L1-L227)

## Domain-Specific Language for Historical Transcription
Kleio notation functions as a domain-specific language (DSL) specifically designed for the challenges of historical document transcription. It addresses the unstructured nature of historical sources by providing a formal syntax that can represent complex relationships, temporal information, and hierarchical document structures. The language is optimized for representing entities such as people, places, objects, and events, along with their attributes and relationships.

The DSL approach allows historians and transcribers to work with a vocabulary that mirrors the conceptual framework of historical research. Terms like "person," "object," "attribute," and "relation" are first-class citizens in the language, making it intuitive for domain experts to use. The syntax supports the representation of uncertainty and ambiguity commonly found in historical sources through optional elements and annotation mechanisms.

Kleio's DSL capabilities extend to handling complex document types such as notarial records, parish registers, and administrative documents. Each document type can be represented using specialized group types that capture the unique structure and content patterns of that document class. This domain-specific focus enables more accurate and consistent transcriptions compared to generic data formats.

**Section sources**
- [dataSyntax.pl](file://src/dataSyntax.pl#L8-L14)
- [struCode.pl](file://src/struCode.pl#L1-L391)

## Common Patterns in Historical Documents
Analysis of actual Kleio transcriptions reveals several common patterns that reflect the structure of different types of historical documents. In baptism records, the pattern typically includes a "bap" (baptism) group containing the child's name, parents' names, godparents, and relevant dates and locations. The structure preserves the hierarchical relationship between these entities, with parents and godparents represented as nested "pn" (father), "mn" (mother), and "god" (godparent) elements.

Notarial documents follow a more complex pattern, with "escritura" (deed) groups containing various participants such as buyers, sellers, witnesses, and officials. These documents often include detailed descriptions of properties, financial terms, and legal relationships. For example, a property sale might include "bem" (property) elements with attributes describing location, value, and characteristics, along with "siza" (tax) elements detailing the financial aspects of the transaction.

Linked data annotations follow a consistent pattern where entities are annotated with references to external authority files. This is particularly evident in biographical collections like the Dehergne Jesuit records, where individuals are linked to Wikidata entries, national library identifiers, and archival sources. The pattern uses the @ symbol followed by the link shortname and identifier (e.g., @wikidata:Q17057616) to establish these connections.

**Section sources**
- [notarial80.cli](file://tests/kleio-home/sources/reference_sources/notariais/notarial80.cli#L1-L800)
- [dehergne-a.cli](file://tests/kleio-home/sources/reference_sources/linked_data/dehergne-a.cli#L1-L800)
- [Devedores.cli](file://tests/kleio-home/sources/reference_sources/varia/Devedores.cli#L1-L72)

## Relationship to Data Model
The Kleio notation syntax is closely tied to an underlying data model defined in structure files, typically in YAML format. This data model specifies the schema that governs valid Kleio transcriptions, including the available groups, elements, their properties, and hierarchical relationships. The structure files define which elements can appear within each group (the "part" parameter), the required elements (the "guaranteed" parameter), and the order of elements (the "position" parameter).

When a Kleio file is processed, the syntax parser validates the transcription against this data model, ensuring that all elements and groups conform to the defined schema. The parser uses the structure information to guide the interpretation of the transcription, determining how to handle each element and its attributes. This tight coupling between syntax and data model ensures consistency across transcriptions and enables automated processing and analysis of the structured data.

The data model also defines semantic types for elements, such as "lingua" (text), "tempora" (date), "numerus" (number), "situs" (location), and "relatio" (relation). These types inform how the data should be interpreted and processed, allowing the system to handle dates, numbers, and other data types appropriately. This semantic layer enhances the utility of the transcribed data for research and analysis.

**Section sources**
- [_sources-structure.yaml](file://tests/kleio-home/structures/_sources-structure.yaml#L1-L800)
- [struCode.pl](file://src/struCode.pl#L1-L391)

## Writing Valid Kleio Syntax
Writing valid Kleio syntax requires adherence to specific rules and conventions. The basic syntax pattern is `element$identifier/attribute1/value1/attribute2/value2`, where elements and attributes are separated by forward slashes. Identifiers should be unique within their context and follow naming conventions that avoid special characters except for permitted ones like hyphens and underscores.

Common pitfalls include incorrect indentation, which breaks the hierarchical structure; missing required elements as defined in the data model; and using invalid element or attribute names. Another common error is improper handling of special characters, particularly in text fields that may contain characters with special meaning in Kleio syntax (like $, /, %, #).

Debugging malformed transcriptions involves checking for syntax errors reported by the parser, validating against the structure schema, and ensuring proper nesting of elements. The system typically provides error messages that indicate the line number and nature of the problem, helping transcribers correct issues efficiently. Using consistent patterns and referring to validated examples can help prevent many common errors.

**Section sources**
- [dataSyntax.pl](file://src/dataSyntax.pl#L54-L62)
- [lexical.pl](file://src/lexical.pl#L104-L111)

## Processing Special Markers
Kleio notation uses several special markers to process different aspects of the transcription. The dollar sign ($) is the primary marker that separates element or group names from their identifiers. The forward slash (/) serves as a delimiter between different components within a line, separating attributes and their values.

Comments are processed using the hash sign (#), with all text following # on a line treated as a comment. Original wording from the source document is preserved using the percent sign (%), allowing transcribers to include verbatim text from the original while adding their own interpretations or transcriptions. These markers are processed during the parsing phase, with the system extracting the appropriate content for each type of annotation.

The backslash (\) is used as an escape character for special symbols that need to be included in text values. Triple quotes (""") can be used to enclose multi-line text blocks, preserving formatting and line breaks. These special markers enable the representation of complex textual content while maintaining the structural integrity of the notation system.

**Section sources**
- [clioPP.pl](file://src/clioPP.pl#L106-L108)
- [dataSyntax.pl](file://src/dataSyntax.pl#L100-L106)

## Conclusion
Kleio notation provides a powerful and flexible system for the structured transcription of historical documents. By combining a domain-specific language with a robust data model, it enables the transformation of unstructured historical sources into structured, analyzable data. The syntax supports hierarchical organization, metadata annotation, and linked data integration, making it suitable for a wide range of historical document types.

The system's design reflects a deep understanding of the challenges faced in historical research, providing tools to handle uncertainty, ambiguity, and complex relationships. Through its formal syntax and validation mechanisms, Kleio ensures consistency and accuracy in transcriptions, while its extensible structure allows adaptation to different types of sources and research needs.

As demonstrated by the various examples in the codebase, from notarial records to biographical collections, Kleio notation can effectively represent diverse historical materials. Its integration of linked data annotations further enhances its value for digital humanities research, enabling connections between transcribed sources and external knowledge bases.