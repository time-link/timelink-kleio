Pure static content served by any HTTP server, split into two audiences:
- `api/` — a self-contained Bootstrap + jQuery page (`index.html`) using the postman-api theme and `bootstrap-treeview.js` to render an interactive sidebar tree of every kleio-server endpoint; CSS/JS assets are shipped in both source and `.min` variants under `api/css` and `api/js`. This is the primary consumer-facing artifact.
- `doc/*.md` — short human-written guides (client setup, linked-data model, structure-file location rules, translation results).
- `markdown/index.md` — a docgen-generated Markdown index listing JSON-RPC-V2 and REST V2 groups as a secondary, machine-readable surface.
- `api.html` at the top level is a thin redirect/shim pointing at `api/index.html`.
There is no build step inside this module; the HTML is pre-generated and committed, so the dependency direction is one-way: consumers read these files, nothing here depends on the Go server at runtime.