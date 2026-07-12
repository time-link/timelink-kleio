---
kind: external_dependency
name: SWI-Prolog Runtime Environment
slug: swi-prolog
category: external_dependency
category_hints:
    - vendor_identity
scope:
    - '**'
---

### SWI-Prolog
- The Timelink-Kleio server is implemented entirely in SWI-Prolog (`serverStart.pl`, `clioStart.pl`, etc.)
- Development requires local SWI-Prolog installation with VSCode VSC-Prolog extension for debugging
- Server runs as a Prolog process listening on port 8088 by default
- Known issue: stable translator has file alias handling problems when processing files without standard `kleio$...` headers (alias already taken errors)
- Docker containerization wraps the Prolog runtime for deployment