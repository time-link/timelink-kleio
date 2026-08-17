# Plan: Fix Issue #45 — Triple-quoted text blocks containing `/` (and `$`) mis-parsed as groups

GitHub issue: https://github.com/time-link/timelink-kleio/issues/45
Branch: `joaquimrcarvalho/issue45`
Worktree: `/Users/jrc/develop/timelink-kleio-worktrees/issue45`
Created: 2026-08-11

## Problem

A `"""..."""` triple-quoted block (e.g. `obs=""" ... """`) spanning multiple lines that contains a line shaped like a group definition — e.g. `wviagem$wicki-v1/7/4/1541/Santiago` — is parsed as a **real** group instead of literal text. It emits spurious groups (auto-IDs `his-1`/`his-2`) and errors about missing guaranteed elements (`ano dia mes`).

Reproduces on `tests/kleio-home/sources/more_sources/wicki-jesuiten-indienfahrer.cli` (L3-49 = `obs="""` block; L41 inside = `wviagem$wicki-v1/...`; L50 outside = the real group).

## Root cause (in `src/dataSyntax.pl`)

- Lexer (`src/lexical.pl:236`) emits only the `"""` delimiter as a `tquote` token; interior text is tokenized normally, so every `/` becomes `(dataflag,2)` regardless of being inside a block.
- Lines are parsed independently one-at-a-time (`src/topLevel.pl:272-285`); the only state carried across lines is the global mutable `tquote`/`dquote` flag.
- `a_line --> group, elements(E)` (L65) is tried **before** `a_line --> elements(E)` (L66). The `group//0` clauses (L68-69) match `<names>$` with **no guard** checking quote state, so even when `tquote` is `true`, a `name$...` line matches `group` → `newGroup/1` fires → the rest is split on `/`. The tquote-guarded literal-text `element` clauses (L76-82) never get a chance. This is the "precedence in the grammar rules" the issue describes.

## Fix (single file, ~2 lines)

In `src/dataSyntax.pl` lines 68-69, add a guard so `group` only fires outside a quoted block:

```prolog
% Before
group-->[(names,N)],dataflag1,{newGroup(N),!}.
group-->fillSpace(__S),[(names,N)],dataflag1,{newGroup(N),!}.

% After
group-->{\+ tquoteOn, \+ dquoteOn},[(names,N)],dataflag1,{newGroup(N),!}.
group-->{\+ tquoteOn, \+ dquoteOn},fillSpace(__S),[(names,N)],dataflag1,{newGroup(N),!}.
```

Use `\+ tquoteOn` (NOT `tquoteOff` — its 2nd clause at L129-130 always succeeds, so it's unsafe as a test). With this guard, an interior `name$.../...` line falls through to `a_line --> elements` (L66), where the existing tquote-guarded `element(storeCore(...))` clauses consume it as literal core text — the intended behavior. Same guard defensively covers multi-line `"..."` blocks.

## Verification

1. **Unit test** — add a case in `src/dataSyntax.pl`'s `:-begin_tests(dataSyntax).` block: open triple quote on one line, include a `name$/a/b/c` line inside, close the quote, then a real `name$/a/b/c` group after; assert no spurious `newGroup` inside the block. Use the existing `lexical:test_lexical` + `get_tokens` + `compile_data` pattern (L156-184).
2. **Semantic test** — run the suite (via `run-semantic-tests` skill) against `wicki-jesuiten-indienfahrer.cli`; confirm the 2 spurious `missing element(s) in wviagem(his-1)`/`his-2` errors are gone and only the real `wviagem` groups (L50+) are emitted.
3. **Regression sweep** — full semantic test suite to confirm no currently-passing fixture regresses (the guard is conservative: it only suppresses `group` recognition while inside a quote block, which is the correct semantics).

## Scope / Risk

- Single-file grammar change + 1 unit test. Low risk: the only behavioral change is that `group` definitions are no longer recognized *inside* an open quote block, which was always the intent (per the comment at L74-75, a `"""` block is "stored as is").
- No changes to lexer, file watcher, CDS, or translation output format.

## Files to edit

- `src/dataSyntax.pl` — guard on `group//0`; new unit test.
- Obsidian note `kleio-issue-45.md` — work log / progress tracking (see below).

## Not in scope

- Issue #33 (generic `transcript`/`nota` part) is a separate enhancement; this fix only addresses the parser bug, but it's what makes the #33 verbatim-transcription patterns work correctly.

## Status

- [x] Plan written
- [ ] Implementation (awaiting user go-ahead after worktree review)
- [ ] Tests passing
