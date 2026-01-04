# Structure Resolution

<cite>
**Referenced Files in This Document**   
- [apiTranslations.pl](file://src/apiTranslations.pl)
- [kleioFiles.pl](file://src/kleioFiles.pl)
- [system.yaml](file://src/stru/system.yaml)
- [sources-structure.yaml](file://src/stru/sources-structure.yaml)
- [groups.yaml](file://src/stru/groups.yaml)
- [elements.yaml](file://src/stru/elements.yaml)
</cite>

## Table of Contents
1. [Introduction](#introduction)
2. [Structure File Resolution Algorithm](#structure-file-resolution-algorithm)
3. [Directory Traversal and Fallback Paths](#directory-traversal-and-fallback-paths)
4. [File Path Resolution with kleioFiles.pl](#file-path-resolution-with-kleiofilespl)
5. [The stru Directory and system.yaml](#the-stru-directory-and-systemyaml)
6. [Inheritance with fons/source Parameter](#inheritance-with-fonssource-parameter)
7. [Best Practices for Structure File Organization](#best-practices-for-structure-file-organization)
8. [Conclusion](#conclusion)

## Introduction
The Timelink-Kleio system employs a sophisticated structure resolution mechanism to determine which STR/YAML file should be used to translate a given Kleio source file. This process involves traversing directory hierarchies, applying fallback paths, and resolving file paths based on naming conventions and directory locations. The algorithm is implemented in the `apiTranslations.pl` module, which searches for structure files by analyzing the source file's path and name, while leveraging the `kleioFiles.pl` module to resolve file paths. The system uses a base structure defined in `system.yaml` that can be extended by more specific structure files, and supports inheritance between structure definitions through the `fons/source` parameter. This document provides a comprehensive analysis of the structure resolution logic, detailing the algorithm, directory traversal, file path resolution, and best practices for organizing structure files.

## Structure File Resolution Algorithm
The structure file resolution algorithm in the Timelink-Kleio system is implemented in the `apiTranslations.pl` module and follows a systematic approach to determine the appropriate STR/YAML file for translating a given Kleio source file. The algorithm begins by checking if a structure file is explicitly specified in the translation request parameters. If not, it proceeds to search for a structure file based on the source file's location and name. The algorithm first attempts to find a structure file with the same name as the source file but with a `.yaml` or `.str` extension in the `structures` directory. If this fails, it looks for a structure file named after the last directory in the source file's path. The algorithm then searches for common structure files like `sources-structure.yaml`, `sources.str`, or `gacto2.str` in both the `sources` and `structures` directories, traversing up the directory hierarchy if necessary. If no specific structure file is found, the algorithm falls back to the default structure file, typically `system.yaml` or `sources-structure.yaml`, which serves as the base structure for all translations. This hierarchical search ensures that the most specific and relevant structure file is used for each source file, while providing a consistent fallback mechanism.

**Section sources**
- [apiTranslations.pl](file://src/apiTranslations.pl#L304-L417)

## Directory Traversal and Fallback Paths
The directory traversal and fallback paths mechanism in the Timelink-Kleio system is designed to efficiently locate the most appropriate structure file for a given Kleio source. The algorithm traverses the directory hierarchy in a depth-first manner, starting from the source file's location and moving up through parent directories. When searching for structure files, the system first checks the `sources` directory and its subdirectories, then the `structures` directory and its subdirectories. For each directory level, the algorithm attempts to match the source file with a structure file using several patterns: it looks for a file with the same base name as the source file but with a `.yaml` or `.str` extension; it searches for a structure file named after the last directory in the path; and it checks for common structure files like `sources-structure.yaml`, `sources.str`, or `gacto2.str`. If no match is found at the current directory level, the algorithm moves up to the parent directory and repeats the search. This fallback mechanism ensures that even if a specific structure file is not available for a particular source, a more general structure can be used. The traversal continues until a matching structure file is found or the root of the directory hierarchy is reached, at which point the system defaults to the base structure file defined in `system.yaml`.

**Section sources**
- [apiTranslations.pl](file://src/apiTranslations.pl#L384-L417)

## File Path Resolution with kleioFiles.pl
The `kleioFiles.pl` module plays a crucial role in resolving file paths for the structure resolution process in the Timelink-Kleio system. This module provides a set of predicates that handle the bidirectional mapping between relative and absolute file paths, taking into account the user's token information and the system's directory structure. The `kleio_resolve_source_file/3` predicate is used to resolve a relative path to an absolute file name, considering the user's source directory as specified in the token information. Similarly, the `kleio_resolve_structure_file/3` predicate resolves relative paths to structure files, using the user's structure directory from the token information. The module also includes the `normalize_str_path/2` predicate, which normalizes structure file paths by considering the current file directory and expanding directory paths. This path resolution mechanism ensures that structure files can be located regardless of their location in the directory hierarchy, and allows for user-specific structure directories to be used. The `kleioFiles.pl` module also handles the creation of structure file paths through the `create_str_path/2` predicate, which resolves special directory names like `system`, `structures`, and `sources` to their actual locations in the file system.

**Section sources**
- [kleioFiles.pl](file://src/kleioFiles.pl#L783-L891)

## The stru Directory and system.yaml
The `stru` directory and `system.yaml` file form the foundation of the structure resolution system in Timelink-Kleio. The `stru` directory, typically located at `KLEIO_HOME/system/conf/kleio/stru`, serves as the central repository for all structure files used in the system. It contains the base structure definitions that can be extended by more specific structure files. The `system.yaml` file, located within the `stru` directory, serves as the base structure that is extended by more specific structure files. This file includes the `groups.yaml` and `elements.yaml` files, which define the core groups and elements used in the Kleio schemas. The `system.yaml` file acts as a template that provides a consistent structure for all translations, ensuring that common elements and groups are available across different source types. Structure files in the `stru` directory can inherit from `system.yaml` using the `source` parameter, allowing for specialization of the base structure while maintaining consistency. The `stru` directory also contains other important structure files like `gacto2.str` and `sources-structure.yaml`, which serve as alternative base structures for different types of sources. This hierarchical organization of structure files in the `stru` directory enables efficient reuse of common definitions and promotes consistency across different translations.

**Section sources**
- [system.yaml](file://src/stru/system.yaml#L1-L4)
- [kleioFiles.pl](file://src/kleioFiles.pl#L641-L667)

## Inheritance with fons/source Parameter
The inheritance mechanism in the Timelink-Kleio system, implemented through the `fons/source` parameter, allows specialized structure definitions to inherit from general ones, promoting code reuse and consistency. When a structure file specifies a `source` parameter, it inherits all the properties and definitions from the referenced structure file, while allowing for overrides and additions. This inheritance is implemented in the `groups.yaml` and `elements.yaml` files, where groups and elements can specify a `source` parameter to inherit from another group or element. For example, the `place` group inherits from the `geoentity` group, inheriting its properties while potentially adding or modifying specific attributes. This mechanism enables the creation of specialized structure files that build upon more general ones, reducing duplication and ensuring consistency across different source types. The inheritance hierarchy is resolved during the structure file processing, with the system recursively loading and merging the definitions from the source structure file. This allows for a flexible and extensible structure system, where new structure files can be created by specializing existing ones, while maintaining a consistent base structure defined in `system.yaml`.

**Section sources**
- [groups.yaml](file://src/stru/groups.yaml#L71-L72)
- [elements.yaml](file://src/stru/elements.yaml#L26-L27)

## Best Practices for Structure File Organization
To ensure proper resolution and avoid conflicts in the Timelink-Kleio system, several best practices should be followed when organizing structure files. First, structure files should be organized in a hierarchical directory structure that mirrors the organization of source files, with a `structures` directory parallel to the `sources` directory. This makes it easier to locate and manage structure files for different types of sources. Second, naming conventions should be consistent, with structure files named after the source files or directories they correspond to, using the `.yaml` extension for new structure files and `.str` for legacy ones. Third, the use of inheritance through the `source` parameter should be encouraged to promote code reuse and consistency, with specialized structure files inheriting from more general ones. Fourth, the `stru` directory should be used for system-wide structure files, while user-specific structure files should be placed in user-specific directories. Fifth, structure files should be kept as simple as possible, with complex structures broken down into smaller, reusable components. Finally, documentation should be included in structure files to explain their purpose and usage, making it easier for other users to understand and modify them.

**Section sources**
- [apiTranslations.pl](file://src/apiTranslations.pl#L339-L417)
- [kleioFiles.pl](file://src/kleioFiles.pl#L855-L891)

## Conclusion
The structure resolution system in Timelink-Kleio is a sophisticated mechanism that ensures the appropriate STR/YAML file is used for translating each Kleio source. By combining directory traversal, fallback paths, and inheritance, the system provides a flexible and extensible framework for managing structure files. The algorithm implemented in `apiTranslations.pl` efficiently locates the most specific structure file for a given source, while falling back to more general structures when necessary. The `kleioFiles.pl` module provides robust file path resolution, enabling the system to handle complex directory hierarchies and user-specific configurations. The `stru` directory and `system.yaml` file serve as the foundation of the structure system, providing a consistent base structure that can be extended by more specific files. The inheritance mechanism through the `fons/source` parameter promotes code reuse and consistency, allowing for the creation of specialized structure files that build upon more general ones. By following best practices for structure file organization, users can ensure proper resolution and avoid conflicts, making the system more maintainable and scalable.