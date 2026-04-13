# Kleio translation services for Timelink (Python)

`kleio-server` provides access to translation services for _Kleio_ files. This is the Python implementation of the Kleio translation server.

## About `Kleio` files

_Kleio_ files are text files with a special notation designed to the transcription of historical sources. The notation was created by Manfred Thaller, as part of the _Kleio historical database system_ (http://web.archive.org/web/20130603204750/http://www.hki.uni-koeln.de/kleio/old.website/).

Although Thaller's Kleio database system is no longer publically available, the notation developped for historical source transcritpion proved very powerfull, providing a concise way for the transcription of complex historical documents.

The _Timelink-Kleio translator_ implements a subset of the Kleio notation designed for the Timelink database system. _Timelink_ provides a set of data models designed for handling person-oriented information collected in historical documents.


This is how a (portuguese) baptism looks like in _Timelink_ Kleio notation:

      bap$b1714-2/12/11714/fl.117v./igreja de sao silvestre/manuel lopes serra (padre)

         celebrante$manuel lopes serra
            ls$profissao/padre

         n$francisca/f

            pn$antonio ferreira
               ls$morada/espinheiro
               ls$freguesia/lousa

            mn$leonarda francisca

            pad$joao fernandes ramalheiro
               ls$morada/moita
               ls$freguesia/lousa

            mad$francisca
               ls$ec/solteira

               pmad$joao goncalves
                  ls$morada/espinheiro
                  ls$freguesia/lousa

Text files implementing the Timelink Kleio notation can be translated by
the _Timelink_ `kleio-server` and imported into the _Timelink_ relational database.

Translation is _intelligent_ in the sense that it operates a _normalization_ of the source information, infering information from the context, and so greatly reducing the overhead of producing normalized data.

_Timelink_ database-services then implement a set of functions that allow the identification of people, reconstruction of biographies, inference of personal networks, and other funcionalities.

For more information on Timelink Kleio Server see: https://github.com/time-link/timelink-kleio


## Services provided by the `kleio-server` API

The API is designed to decouple Kleio source handling from other software components. It provides funcionality to translate source files, inspect results of translation for errors or warnings, obtain the generated data in XML format. It also allows basic file management and basic git operations, so that it can be used to isolate other software components from directly handling file related operations.

Main services privided by the API are:

* translations: translates kleio source files.
* sources: lists available sources for translation.
* file management services: including downloading, uploading and deleting files (sources and structures), creating,copying, moving and deleting directories.
* permission management using tokens: generate_token, invalidate_token,invalidate_use: magament of 'authorization' with token.
* basic git interaction: fetch, pull, commit, push.

API documentation is available in [docs/api](docs/api/index.html)

## Api documentation

API documentation is available at [docs/api/index.html](docs/api/index.html)

## Running the server

### Installation

Install the package:

```bash
pip install -e .
```

For development, install with dev dependencies:

```bash
pip install -e ".[dev]"
```

### Running locally

Start the server with uvicorn:

```bash
uvicorn kleio.api.app:app --host 0.0.0.0 --port 8088
```

Or with auto-reload for development:

```bash
uvicorn kleio.api.app:app --reload --port 8088
```

### Running with Docker

Build the Docker image:

```bash
docker build -f Dockerfile.python -t kleio-server-python .
```

Run the container:

```bash
docker run -v $(PWD):/kleio-home -p 8088:8088 -d kleio-server-python
```

### Configuration

The server is configured through environment variables:

* `KLEIO_ADMIN_TOKEN`: Administrative access token. If not set, a token is generated on startup.
* `KLEIO_HOME_DIR`: Base directory for Kleio files (default: `/kleio-home`).
* `KLEIO_DEBUG`: Set to "true" to enable debug logging.
* `KLEIO_CORS_SITES`: Comma-separated list of allowed CORS sites, or "*" for all.

Example with Docker:

```bash
docker run -v $(PWD):/kleio-home -e KLEIO_ADMIN_TOKEN=myprivatetoken -p 8088:8088 -d kleio-server-python
```

If `KLEIO_ADMIN_TOKEN` is not set, the server generates a token with admin privileges which can be obtained from the `.kleio.json` file in the directory mapped to `/kleio-home`.

### Running under current user

On Linux systems, Docker runs under the root user by default. To run as the current user:

```bash
docker run -v $PWD:/kleio-home -u $(id -u):$(id -g) -p 8088:8088 -d kleio-server-python
```

## Development

### Requirements

* Python 3.11 or higher
* pip

### Dependencies

* FastAPI - Web framework for building APIs
* uvicorn - ASGI server
* pydantic - Data validation using Python type annotations
* lxml - XML processing
* PyYAML - YAML parser
* gitpython - Git repository interaction
* httpx - HTTP client

### Running locally for development

```bash
# Install development dependencies
pip install -e ".[dev]"

# Run with auto-reload
uvicorn kleio.api.app:app --reload --port 8088
```

### Tests

Run the test suite:

```bash
python -m pytest tests/ -v
```

Run specific tests:

```bash
python -m pytest tests/test_parser.py -v
python -m pytest tests/test_inference.py -v
```

### Recommended Tools

* VSCode with Python extension
* Postman for API testing (directory `api/postman` contains exported collections)

## Project Structure

The Python implementation follows this module layout:

```
kleio/
├── api/              # FastAPI REST API
│   ├── app.py        # Application factory and routes
│   ├── auth.py       # Authentication and token management
│   └── routes/       # Endpoint handlers
├── parser/           # Kleio notation parser
│   ├── lexer.py      # Tokenizer
│   ├── syntax.py     # Syntax analysis
│   ├── builder.py    # Group/element tree builder
│   └── models.py     # Parser data models
├── schema/           # YAML structure file handling
│   ├── loader.py     # Structure file loader
│   ├── models.py     # Schema data models
│   └── registry.py   # Structure registry
├── inference/        # Rule engine
│   ├── engine.py     # Inference engine
│   ├── rules.py      # Rule definitions
│   ├── loader.py     # Rule loader
│   └── models.py     # Inference data models
├── export/           # Data exporters
│   ├── xml_exporter.py   # XML output
│   └── json_exporter.py  # JSON output
├── config.py         # Configuration management
├── errors.py         # Error accumulation
├── mappings.py       # Data normalization mappings
├── persistence.py    # Property store
└── linked_data.py    # Linked data resolution
```

## Documentation

Additional documentation is available in the `docs/doc` directory:

* [Inference Rules](docs/doc/inference_rules.md) - YAML format for inference rules
* [Mappings](docs/doc/mappings.md) - YAML format for data mappings
* [Linked Data](docs/doc/linked_data.md) - Linked data notation
* [Structure File Location](docs/doc/stru_file_location.md) - How structure files are located
* [Translation Results](docs/doc/translation_results.md) - Translation results format
* [Client Setup](docs/doc/client_setup.md) - Setting up clients to use the API

## Version

This is version 0.1.0, the initial Python port of the Kleio translation server. The original implementation was written in SWI-Prolog.
