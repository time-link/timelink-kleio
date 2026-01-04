# Advanced Topics

<cite>
**Referenced Files in This Document**   
- [inference.pl](file://src/inference.pl)
- [mappings.pl](file://src/mappings.pl)
- [apiIdentifications.pl](file://src/apiIdentifications.pl)
- [reports.pl](file://src/reports.pl)
- [inference_sample.yml](file://tests/kleio-home/inferences/inference_sample.yml)
- [person-mapping.yml](file://tests/kleio-home/mappings/person-mapping.yml)
- [sample-mapping.yml](file://tests/kleio-home/mappings/sample-mapping.yml)
- [AGENTS.md](file://AGENTS.md)
</cite>

## Table of Contents
1. [Custom Inference Rules Implementation](#custom-inference-rules-implementation)
2. [Mapping System for Data Transformation](#mapping-system-for-data-transformation)
3. [Advanced Identification Resolution Techniques](#advanced-identification-resolution-techniques)
4. [Custom Report Generation](#custom-report-generation)
5. [Extending the System](#extending-the-system)
6. [Complex Scenario Examples](#complex-scenario-examples)
7. [Performance and Scalability](#performance-and-scalability)
8. [Integration Patterns](#integration-patterns)

## Custom Inference Rules Implementation

The inference engine in timelink-kleio enables contextual data enrichment during translation through a sophisticated rule-based system. The core implementation is located in `inference.pl`, which defines a domain-specific language for creating inference rules that automatically generate relations and attributes based on patterns in the source data.

The inference system operates by matching path patterns in the hierarchical structure of Kleio notation and applying actions when conditions are met. Each rule follows the format `if PATH then ACTION`, where PATH consists of elements like `sequence(C)`, `group(Name,ID)`, `extends(Class,ID)`, or `clause(C)`. These path elements allow the system to navigate the document structure and identify specific patterns. When a rule is triggered, it can generate relations between entities, add attributes to entities, or reset the current scope.

The system implements extensive family relationship inference, automatically deducing parent-child relationships, marital connections, and ancestral links up to multiple generations. For example, when encountering a male actor (`actorm`) with a father (`pai`) reference, the system automatically creates a "parentesco" relation of type "pai" between the parent and child entities. Similar rules exist for mothers, wives, husbands, and previous marriages, with special handling for cases where individuals are referenced with different gender indicators.

The inference rules also handle complex genealogical patterns, including multi-generational relationships through prefixes like `ppai` (grandfather), `pppai` (great-grandfather), and their maternal counterparts. These rules can operate on both hierarchical and "flat" act structures, providing flexibility in how source documents are organized. The system also resolves couples from parent references, automatically creating marital relationships and setting marital status attributes.

For domain-specific extensions, the system supports YAML-based inference rule definitions as demonstrated in `inference_sample.yml`. This allows non-programmers to define inference logic using a more accessible format while maintaining the same expressive power as the Prolog implementation.

**Section sources**
- [inference.pl](file://src/inference.pl#L1-L800)
- [inference_sample.yml](file://tests/kleio-home/inferences/inference_sample.yml#L1-L100)

## Mapping System for Data Transformation

The mapping system in timelink-kleio provides a comprehensive framework for transforming data between different schemas, enabling the translation of source-oriented models into person-oriented data models. Implemented in `mappings.pl`, this system defines how groups in the source notation map to entities in the target database schema.

The mapping language uses a declarative syntax with operators like `mapping`, `class`, `super`, `table`, and `attributes` to define the transformation rules. Each mapping connects a source group name to a target class, specifying the database table and column mappings. The system supports inheritance through the `super` keyword, allowing classes to extend existing entity types and inherit their attributes.

The implementation includes mappings for various entity types such as persons, objects, relations, attributes, and specialized historical records like marriages (`cas`), baptisms (`bap`), and property records (`bem`). Each mapping specifies the database table and detailed column definitions, including data types, sizes, precision, and primary key constraints. For example, the person mapping defines fields for ID, name, sex, and observation text, with appropriate database types and constraints.

The system also handles complex entity relationships through foreign key mappings and supports both simple and compound data types. Mappings can transform hierarchical source structures into normalized relational database schemas, resolving references and creating appropriate indexes for efficient querying. The implementation includes specialized mappings for temporal records with day, month, and year components, as well as geographic entities with location and type information.

For flexible configuration, the system supports YAML-based mapping definitions as shown in `person-mapping.yml` and `sample-mapping.yml`. These files provide a human-readable format for defining mappings that can be easily modified without changing the core Prolog code. The YAML format supports documentation fields, making it easier to understand the purpose and usage of each mapping.

The mapping system is designed to be extensible, allowing new entity types to be added by defining additional mappings. This enables the system to adapt to different historical document types and research requirements without modifying the core translation engine.

**Section sources**
- [mappings.pl](file://src/mappings.pl#L1-L518)
- [person-mapping.yml](file://tests/kleio-home/mappings/person-mapping.yml#L1-L15)
- [sample-mapping.yml](file://tests/kleio-home/mappings/sample-mapping.yml#L1-L24)

## Advanced Identification Resolution Techniques

The identification resolution system in timelink-kleio, implemented in `apiIdentifications.pl`, provides sophisticated capabilities for managing and retrieving identification files that link historical entities to external authority records. This system enables the resolution of ambiguous historical references by connecting entities in the database to standardized identifiers from external sources.

The API provides endpoints for retrieving identification files (typically named `mhk_identification*.cli`) that contain mappings between local entity IDs and external identifiers. The system uses token-based authentication to control access to these files, ensuring that only authorized users can retrieve sensitive identification data. The implementation includes methods for both direct file retrieval and directory listing, supporting both individual file access and batch operations.

The identification resolution process involves several key components: file path resolution, access control, and content delivery. When a request is made, the system first resolves the absolute path of the requested identification file using the user's token information to determine their source home directory. It then applies access control checks to ensure the requesting user has appropriate permissions before serving the file.

The system supports recursive directory traversal for finding identification files, allowing users to search entire directory trees for relevant identification data. Results can be returned as direct file downloads or as JSON-formatted lists of available files, providing flexibility in how clients consume the data. The implementation also supports URL generation for identification files, enabling clients to construct direct download links.

For enterprise deployments, the identification system integrates with the broader security framework, using the same token management infrastructure as other API components. This ensures consistent authentication and authorization across the entire system. The design allows for both local and distributed identification repositories, supporting collaborative research environments where multiple institutions contribute to a shared identification database.

**Section sources**
- [apiIdentifications.pl](file://src/apiIdentifications.pl#L1-L105)

## Custom Report Generation

The reporting system in timelink-kleio, implemented in `reports.pl`, provides flexible capabilities for generating custom reports from translation results and system operations. This system enables users to create specialized outputs for analysis, validation, and archival purposes.

The core reporting functionality is built around the `prepare_report/1` and `report/1` predicates, which allow output to be directed to both files and console simultaneously. The system supports configurable output options, including whether to send output to the console, enabling users to control verbosity based on their needs. Reports can include version information, timestamps, and structured data in various formats.

The implementation uses a meta-predicate approach with `rep_call/1` to execute reporting operations, providing a flexible framework for extending report content. This design allows custom reporting logic to be integrated seamlessly with the core system. The reporting system handles file operations, including opening, writing, and closing report files, with appropriate error handling and permission management.

For enterprise deployments, the reporting system supports batch operations and can be integrated with automated workflows. Reports can be generated programmatically as part of translation pipelines, providing detailed logs of processing activities, error conditions, and statistical summaries. The system also supports incremental reporting, allowing large datasets to be processed and reported on in manageable chunks.

The design emphasizes reliability and data integrity, with proper file handling and error recovery mechanisms. Report files are created with appropriate permissions and are flushed to disk regularly to prevent data loss. The system also provides status monitoring through `report_status/1`, enabling clients to check the current reporting state and manage report generation dynamically.

**Section sources**
- [reports.pl](file://src/reports.pl#L1-L136)

## Extending the System

timelink-kleio is designed with extensibility in mind, allowing developers and researchers to enhance its capabilities for specific domains and use cases. The system supports several extension points, enabling the creation of domain-specific rules, new processing agents, and integration with external systems.

Custom inference rules can be added to handle domain-specific patterns in historical documents. By extending the rules in `inference.pl` or creating new YAML-based rule files, users can implement specialized logic for particular types of sources or research questions. For example, in ecclesiastical archives, custom rules could infer hierarchical relationships within church organizations or resolve complex tithe records.

New processing agents can be developed by adding predicates to existing modules or creating new API endpoints. The system's modular architecture, as documented in `AGENTS.md`, provides a framework for autonomous components that can operate independently to process and manage historical data. Developers can create agents for specialized tasks such as paleographic analysis, linguistic normalization, or automatic citation generation.

The mapping system can be extended to support new data schemas and transformation requirements. By adding new mappings in `mappings.pl` or creating YAML mapping files, users can adapt the system to work with different database schemas or export formats. This enables integration with external research databases, digital humanities platforms, or institutional repositories.

Integration with external systems is facilitated through the REST API and token-based authentication. The system can be connected to digital asset management systems, scholarly publishing platforms, or collaborative research environments. For example, identification data could be synchronized with authority files from national libraries or linked data repositories like Wikidata.

The extension system follows established threading and safety patterns, ensuring that new components integrate smoothly with the existing architecture. Developers are encouraged to follow the patterns demonstrated in the core codebase, particularly in areas like error handling, resource management, and concurrent processing.

**Section sources**
- [inference.pl](file://src/inference.pl#L1-L800)
- [mappings.pl](file://src/mappings.pl#L1-L518)
- [AGENTS.md](file://AGENTS.md#L136-L144)

## Complex Scenario Examples

The advanced capabilities of timelink-kleio enable the resolution of complex historical research scenarios that would be challenging to address with conventional data processing tools. These scenarios demonstrate the system's ability to handle ambiguity, integrate diverse data sources, and generate meaningful insights from historical records.

One complex scenario involves resolving ambiguous historical references in medieval documents where individuals are identified by name and location but without unique identifiers. The system can use contextual information from surrounding records, combined with inference rules about familial relationships and social structures, to disambiguate references. For example, when multiple individuals share the name "João Fernandes" in a parish register, the system can analyze baptismal, marriage, and burial records to establish family trees and distinguish between individuals based on their relationships and life events.

Another scenario involves generating specialized reports for prosopographic studies. Researchers studying social networks in early modern Portugal can create custom reports that extract all references to individuals with specific occupations, such as "mercador" (merchant) or "clérigo" (clergyman), and analyze their relationships, property holdings, and geographic distribution. The reporting system can generate network visualizations, statistical summaries, and detailed biographical sketches from the normalized data.

A third scenario involves integrating data from multiple archival collections with different structural conventions. The mapping system can transform heterogeneous source formats into a unified schema, while custom inference rules resolve inconsistencies in naming conventions and dating systems. For example, when combining parish registers from different dioceses with varying formats for recording marriages, the system can normalize the data and create a comprehensive view of marital networks across regions.

These complex scenarios demonstrate how the combination of inference rules, mapping transformations, identification resolution, and custom reporting enables sophisticated historical analysis. The system's ability to enrich sparse data through contextual inference and transform diverse sources into a coherent dataset makes it particularly valuable for large-scale historical research projects.

**Section sources**
- [inference.pl](file://src/inference.pl#L1-L800)
- [mappings.pl](file://src/mappings.pl#L1-L518)
- [apiIdentifications.pl](file://src/apiIdentifications.pl#L1-L105)
- [reports.pl](file://src/reports.pl#L1-L136)

## Performance and Scalability

The timelink-kleio system is designed with performance and scalability considerations for enterprise deployments handling large historical datasets. The architecture balances computational efficiency with the complex requirements of historical document processing.

The inference engine is optimized to minimize redundant pattern matching through careful rule ordering and indexing. Rules are organized to handle common patterns first, reducing the average processing time for typical documents. The system also employs caching mechanisms for frequently accessed data, such as identification files and structure definitions, to reduce disk I/O operations.

For scalability, the system supports parallel processing through its threading model, allowing multiple translation tasks to be executed concurrently. This enables efficient utilization of multi-core processors and supports high-throughput processing in server environments. The REST API design allows for horizontal scaling, with multiple server instances handling client requests behind a load balancer.

Memory management is carefully handled, particularly for large documents and complex transformations. The system uses incremental processing where possible, avoiding the need to load entire datasets into memory. Database operations are optimized with appropriate indexing and batch processing to maintain performance as dataset sizes grow.

The system's performance characteristics make it suitable for enterprise deployments processing thousands of historical documents. Typical bottlenecks occur in complex inference operations and large-scale data transformations, which can be mitigated through hardware scaling and careful rule optimization. The system provides logging and monitoring capabilities to identify performance issues and optimize processing workflows.

**Section sources**
- [inference.pl](file://src/inference.pl#L1-L800)
- [mappings.pl](file://src/mappings.pl#L1-L518)
- [apiIdentifications.pl](file://src/apiIdentifications.pl#L1-L105)
- [reports.pl](file://src/reports.pl#L1-L136)

## Integration Patterns

timelink-kleio supports various integration patterns for enterprise deployments, enabling seamless interaction with other systems in digital humanities and archival environments. These patterns leverage the system's REST API, modular architecture, and extensible design.

One common integration pattern involves connecting the system to digital repository platforms. Through the file management and translation APIs, digital archives can automatically process newly ingested Kleio notation files, generating normalized data for indexing and discovery. The system can be configured to trigger translation workflows when new files are added to specific directories, enabling automated processing pipelines.

Another pattern involves integration with research collaboration platforms. The identification resolution system can be connected to shared authority files, allowing multiple research teams to contribute to and benefit from a common pool of entity identifications. This supports collaborative prosopographic studies where researchers from different institutions work on related historical datasets.

For publishing workflows, the system can be integrated with scholarly publishing platforms. Custom report generation can produce structured data outputs in formats suitable for journal supplements or data publications. The system's ability to generate consistent, normalized data ensures that published datasets meet scholarly standards for reproducibility and reuse.

The token-based authentication system enables secure integration with institutional identity management systems. User tokens can be synchronized with institutional credentials, providing single sign-on capabilities and role-based access control. This allows organizations to manage access to historical data according to their security policies.

These integration patterns demonstrate how timelink-kleio can serve as a central component in larger digital humanities ecosystems, transforming raw historical transcriptions into structured, reusable data that can be shared across multiple platforms and research projects.

**Section sources**
- [apiIdentifications.pl](file://src/apiIdentifications.pl#L1-L105)
- [reports.pl](file://src/reports.pl#L1-L136)
- [AGENTS.md](file://AGENTS.md#L1-L144)