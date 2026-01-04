# Custom Reporting

<cite>
**Referenced Files in This Document**   
- [reports.pl](file://src/reports.pl)
- [topLevel.pl](file://src/topLevel.pl)
- [errors.pl](file://src/errors.pl)
- [persistence.pl](file://src/persistence.pl)
- [utilities.pl](file://src/utilities.pl)
- [logging.pl](file://src/logging.pl)
- [apiReports.pl](file://src/apiReports.pl)
</cite>

## Table of Contents
1. [Introduction](#introduction)
2. [Core Reporting Functions](#core-reporting-functions)
3. [Report Configuration and Options](#report-configuration-and-options)
4. [Report Status Management](#report-status-management)
5. [Integration with Top-Level Processing](#integration-with-top-level-processing)
6. [Error Reporting and Validation](#error-reporting-and-validation)
7. [Data Flow and Processing](#data-flow-and-processing)
8. [API Integration](#api-integration)
9. [Usage Patterns and Examples](#usage-patterns-and-examples)
10. [Common Issues and Solutions](#common-issues-and-solutions)

## Introduction

The custom reporting system in the timelink-kleio project provides a comprehensive mechanism for capturing translation output, logging processing metadata, and generating audit trails. The system is centered around the `reports.pl` module, which implements a dual-output capability that simultaneously writes to both file and console destinations. This reporting infrastructure is tightly integrated with the translation process, capturing translation results, validation messages, and processing metadata throughout the document processing lifecycle.

The reporting system serves multiple purposes: it provides real-time feedback during translation operations, creates persistent records for quality assurance and auditing, and supports debugging complex translation issues. The system is designed to be flexible and configurable, allowing different reporting modes based on the specific requirements of the translation task.

**Section sources**
- [reports.pl](file://src/reports.pl#L1-L136)
- [topLevel.pl](file://src/topLevel.pl#L1-L286)

## Core Reporting Functions

The reporting system is built around four primary functions that manage the complete reporting lifecycle: `prepare_report/1,2`, `report/1`, and `close_report_file/0`. These functions work together to establish reporting contexts, capture output, and properly clean up resources.

### prepare_report/1,2

The `prepare_report` predicate initializes the reporting system and configures the output destination. It has two forms: a single-argument version and a two-argument version with options.

The single-argument form `prepare_report(+ReportFile)` is provided for backward compatibility and automatically configures dual output to both file and console. This function calls the two-argument version with the `type(console)` option, ensuring that all output is mirrored to the console for immediate feedback.

The two-argument form `prepare_report(+ReportFile,+Options)` provides full configuration control through an options list. The primary option is `type(console)` which enables console output alongside file output. When this option is omitted, output is directed only to the file. The function sets up the file handle, configures the output mode, and writes an initial header containing version information before activating the reporting system.

**Section sources**
- [reports.pl](file://src/reports.pl#L27-L57)

### report/1

The `report/1` predicate is the primary mechanism for generating report content. It accepts a list of predicates to execute, captures their output, and directs it to the configured destinations. When reporting is active, the system captures the output as a string and writes it to both the report file and optionally to the console.

The function first checks the current reporting status using `report_status/1`. If reporting is disabled, it simply executes the predicates without capturing output. When reporting is enabled, it uses `with_output_to/3` to capture the output of the predicate list as a string, writes this string to the report file, and if configured, also outputs it to the console using `format/2`.

The predicate list can include various types of output operations such as `write/1`, `writeln/1`, or calls to other predicates that generate output. This flexibility allows complex reporting content to be constructed programmatically.

**Section sources**
- [reports.pl](file://src/reports.pl#L84-L110)

### close_report_file/0

The `close_report_file/0` predicate terminates the current reporting session and properly closes the report file. This function is essential for ensuring that all buffered output is flushed to disk and that file handles are released.

The function retrieves the current report file name from the system state using `get_value/2`, logs a debug message indicating the file is being closed, and then calls `close_file/1` to close the file handle. Proper use of this function prevents resource leaks and ensures report files are complete and properly terminated.

**Section sources**
- [reports.pl](file://src/reports.pl#L117-L125)

## Report Configuration and Options

The reporting system provides flexible configuration options through the `prepare_report/2` function, allowing fine-grained control over reporting behavior.

### Output Mode Configuration

The primary configuration option is the `type` parameter, which controls where output is directed. The `type(console)` option enables dual output to both file and console, providing immediate feedback during processing while maintaining a persistent record. When this option is omitted, output is directed only to the file, which can be useful for automated processing where console output is not needed.

The system also supports the `type(noconsole)` option for code clarity, though the actual determination of console output depends on the presence of `type(console)`. This design maintains backward compatibility while providing explicit control over output destinations.

### File Permissions and Access

When creating report files, the system attempts to set appropriate file permissions using `chmod/2` to ensure the files are accessible. The `set_report_file/1` function handles file creation and permission setting, with error handling to log any issues that occur during permission modification. This ensures that report files are created with appropriate access controls while maintaining system stability if permission changes fail.

**Section sources**
- [reports.pl](file://src/reports.pl#L47-L57)
- [reports.pl](file://src/reports.pl#L62-L66)

## Report Status Management

The reporting system includes functions for managing and querying the current reporting state, providing control over when reporting is active.

### set_report/1

The `set_report/1` predicate controls whether reporting is currently active. It accepts three possible values: `on`, `off`, or a variable value. When set to `on`, subsequent calls to `report/1` will capture and direct output according to the current configuration. When set to `off`, `report/1` calls will execute the predicates but not capture or redirect their output.

This function uses `put_value/2` to store the current status in the system state, making it accessible to other components. The function is designed to be idempotent and includes validation to handle invalid input values gracefully.

### report_status/1

The `report_status/1` predicate queries the current reporting state. It can be used to determine whether reporting is currently enabled before generating potentially expensive report content. The function handles both input and output modes: when given a variable, it returns the current status; when given a specific value, it checks if the current status matches.

This function is used internally by `report/1` to determine how to handle output, but can also be called directly by other components that need to conditionally generate report content based on the current reporting state.

**Section sources**
- [reports.pl](file://src/reports.pl#L68-L83)

## Integration with Top-Level Processing

The reporting system is deeply integrated with the top-level translation processing functions in `topLevel.pl`, providing consistent reporting throughout the translation workflow.

### Initialization and Setup

During system initialization in `clio_init/0`, reporting is explicitly disabled by calling `set_report(off)`. This ensures a clean state at startup, with reporting only activated when specifically requested for a translation task. This design prevents unintended output during system initialization and allows each translation session to independently configure its reporting needs.

### Structure and Data Processing

The `stru/1` and `dat/1` predicates use the reporting system extensively to provide feedback during processing. When processing a structure file, `stru/1` generates reports that include the filename being processed, error counts, and completion messages. Similarly, `dat/1` generates reports for data file processing, including progress indicators and final status.

These functions use `report/1` with predicate lists that include `write/1`, `writeln/1`, and calls to other reporting predicates like `perror_count/0`. This integration provides a consistent user experience across different types of processing tasks.

**Section sources**
- [topLevel.pl](file://src/topLevel.pl#L94)
- [topLevel.pl](file://src/topLevel.pl#L102-L130)
- [topLevel.pl](file://src/topLevel.pl#L139-L159)

## Error Reporting and Validation

The reporting system works in conjunction with the error handling system to provide comprehensive feedback on translation issues.

### Error Output Integration

The `errors.pl` module uses the reporting system to output error and warning messages. Functions like `error_out/1,2` and `warning_out/1,2` use `report/1` to include error messages in the report output. This ensures that all errors are captured in the persistent report file while also being displayed on the console.

The `p_error_warn_context/2,3` predicate constructs detailed error messages that include the error type, source file, line number, and surrounding context. This rich contextual information is invaluable for debugging translation issues and understanding the circumstances under which errors occurred.

### Error Counting and Reporting

The system maintains counters for errors and warnings using the `counters.pl` module. The `perror_count/0` predicate outputs the current error and warning counts, providing a summary of translation quality. This function is typically called at the end of processing to give users immediate feedback on the success of the translation.

The `check_continuation/0` predicate uses the error count to determine whether processing should continue when a high number of errors have been encountered. This prevents the system from continuing processing when the input data is severely problematic, avoiding potentially infinite error generation.

**Section sources**
- [errors.pl](file://src/errors.pl#L77-L113)
- [errors.pl](file://src/errors.pl#L200-L208)

## Data Flow and Processing

The reporting system implements a sophisticated data flow mechanism that captures output from multiple sources and directs it to the appropriate destinations.

### Output Capture Mechanism

The system uses Prolog's `with_output_to/3` predicate to capture the output of predicate lists as strings. This allows the reporting system to intercept output that would normally go to the standard output stream and redirect it to the report file. The captured string is then written to the file using `write/2` and optionally to the console using `format/2`.

This approach allows the reporting system to capture output from any predicate that generates text output, regardless of how that output is generated. It provides a transparent mechanism for redirecting output without requiring changes to the predicates being called.

### State Management

The reporting system uses the `persistence.pl` module to maintain its state across different parts of the system. Key values such as the current report file name (`report`), report status (`rep_stat`), and report type (`report_type`) are stored using `put_value/2` and retrieved using `get_value/2`. This state management approach allows different components to access the current reporting configuration without requiring explicit parameter passing.

The use of thread-local storage ensures that multiple translation processes can run concurrently without interfering with each other's reporting state.

**Section sources**
- [reports.pl](file://src/reports.pl#L95-L103)
- [persistence.pl](file://src/persistence.pl#L31-L47)

## API Integration

The reporting system is accessible through the API, allowing external systems to retrieve report files and integrate reporting functionality into larger workflows.

### apiReports Module

The `apiReports.pl` module provides API endpoints for accessing report files. The `reports/5` predicate handles requests for report files, delegating to the `sources/5` predicate for file retrieval. This integration allows clients to retrieve report files through the REST API, enabling programmatic access to translation reports.

The `reports_get/3` predicate provides a JSON interface for retrieving reports, allowing integration with web-based clients and other systems that consume JSON data. This function extracts the path parameter from the request and forwards it to the main reports function.

### File Retrieval

The system stores report files in a structured directory, with filenames that include timestamps to ensure uniqueness. The API allows clients to retrieve these files by path, enabling automated processing of report content for quality assurance, change tracking, and audit purposes.

**Section sources**
- [apiReports.pl](file://src/apiReports.pl#L14-L19)

## Usage Patterns and Examples

The reporting system supports several common usage patterns for different translation scenarios.

### Quality Assurance Reports

For quality assurance, the system can generate detailed reports that capture all translation output, including errors, warnings, and processing metadata. By enabling console output, users can monitor progress in real-time while the complete record is saved for later analysis.

```prolog
% Example usage pattern
prepare_report('qa_report.txt', [type(console)]),
% Perform translation operations
% ...
perror_count, % Report error summary
close_report_file.
```

### Change Tracking

The timestamped report files in the reports directory can be used for change tracking, comparing outputs from different translation runs to identify changes in behavior or output quality. The diff files in the reports directory suggest that automated comparison of report outputs is a common practice.

### Audit Trails

For audit purposes, the system can generate comprehensive reports with console output disabled, creating clean, persistent records of translation activities. These reports include version information, timestamps, and complete processing logs, providing a verifiable record of translation operations.

**Section sources**
- [reports.pl](file://src/reports.pl#L59-L61)
- [reports/](file://src/reports/)

## Common Issues and Solutions

### Report File Not Closing Properly

If report files are not properly closed, it may indicate that `close_report_file/0` was not called. Ensure that every `prepare_report/1,2` call is paired with a corresponding `close_report_file/0` call, ideally using exception handling to guarantee cleanup.

### Missing Console Output

If console output is expected but not appearing, verify that `type(console)` was included in the options for `prepare_report/2`. The default single-argument version automatically includes this option, but the two-argument version requires explicit specification.

### Permission Errors

If report files cannot be created due to permission errors, check that the target directory is writable and that the system has appropriate permissions. The system attempts to set group write permissions, which may fail in restricted environments.

### Performance Issues with Large Reports

For very large reports, the in-memory buffering of output strings may impact performance. Consider processing large outputs in smaller chunks or using direct file writing for extremely large datasets.

**Section sources**
- [reports.pl](file://src/reports.pl#L64)
- [reports.pl](file://src/reports.pl#L123-L125)
- [reports.pl](file://src/reports.pl#L103)