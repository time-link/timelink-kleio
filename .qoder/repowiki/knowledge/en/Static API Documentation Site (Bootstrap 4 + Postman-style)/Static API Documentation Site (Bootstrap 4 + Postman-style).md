---
kind: frontend_style
name: Static API Documentation Site (Bootstrap 4 + Postman-style)
category: frontend_style
scope:
    - '**'
source_files:
    - docs/api/index.html
    - docs/api/css/main.min.css
    - docs/api/css/sidebar-themes.css
    - docs/api/css/postman.min.css
    - docs/api/js/main.min.js
    - docs/api/js/bootstrap-treeview.js
    - api/postman/api.json
---

The repository contains no application UI — the only frontend is a static, generated API documentation site under `docs/api/`. It is built from a Postman collection and rendered as a single large HTML page styled with Bootstrap 4.3.1 plus a Postman-inspired theme.

**System / approach**
- Static HTML site generated from `api/postman/*.json` via a Postman-to-HTML generator.
- CSS framework: Bootstrap v4.3.1 (bundled minified in `docs/api/css/main.min.css`).
- Theme layer: `sidebar-themes.css` provides light/dark sidebar theming toggled via the `.light-theme` class on `<body>`.
- Icons: Font Awesome 5.8.2 loaded from CDN.
- Sidebar navigation: `bootstrap-treeview.js` renders the API tree; custom scrollbar via `jquery.mCustomScrollbar`.
- No build toolchain (no Webpack, Tailwind, SCSS compiler); styles are plain CSS shipped as both source and `.min.css` variants.

**Key files**
- `docs/api/index.html` — single-page entrypoint that wires all CSS/JS assets and hosts the rendered API content.
- `docs/api/css/main.min.css` — compiled Bootstrap 4.3.1 base stylesheet.
- `docs/api/css/sidebar-themes.css` — light/dark sidebar theme overrides.
- `docs/api/css/postman.min.css`, `postman-api.css` — Postman-style layout for request/response panels.
- `docs/api/js/main.min.js`, `main.js` — runtime behavior (tree rendering, snippet expansion, modal).
- `docs/api/js/bootstrap-treeview.js` — dependency for the left-hand API tree.
- `api/postman/*.json` — source of truth for endpoints consumed by the generator.

**Architecture & conventions**
- The site is purely static; there is no client-side SPA or component library.
- Visual tokens (colors, breakpoints, fonts) come from Bootstrap's CSS variables in `main.css`; theme tweaks live in `sidebar-themes.css` rather than a separate design-token file.
- Responsive strategy follows Bootstrap's grid (`col-md-*`, `col-xs-*`) and media queries defined inside the bundled CSS.
- The body element carries a `page-wrapper toggled light-theme` class to control sidebar state and theme at load time.

**Rules developers should follow**
- Do not add inline styles to `index.html`; extend `sidebar-themes.css` or create new CSS files under `docs/api/css/`.
- Prefer Bootstrap utility classes already available in the bundled `main.min.css` over writing custom rules.
- If adding new visual tokens, centralize them in `sidebar-themes.css` using CSS variables rather than hard-coding hex values across selectors.
- Keep any new JS under `docs/api/js/` and reference it from `index.html`; avoid pulling in additional CDNs unless necessary.