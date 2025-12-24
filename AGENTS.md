# Repository Guidelines

## Project Structure & Module Organization

- `toon-mode.el`: Emacs major mode implementation (syntax table, font-lock, indentation).
- `test/toon-mode-tests.el`: ERT test suite.
- `Makefile`: Convenience target for running tests.
- `readme.org`: Short project overview and upstream links.

## Build, Test, and Development Commands

This repo is a single Emacs Lisp file; there is no build pipeline.

- Load in Emacs (manual smoke test): `M-x load-file` -> `toon-mode.el`.
- Batch load check: `emacs -Q --batch -l toon-mode.el` (exits non-zero if load fails).
- Run tests: `make test`.
- Try the mode: open a `.toon` file and verify indentation and highlighting.

## Coding Style & Naming Conventions

- Emacs Lisp conventions apply (2-space indentation; no tabs).
- Prefix all public symbols with `toon-` (e.g., `toon-indent-line`, `toon-font-lock-keywords`).
- Keep docstrings concise and user-facing; prefer `defcustom` for user-tunable settings.
- Avoid non-ASCII unless required by the TOON spec or Emacs conventions.

## Testing Guidelines

- Automated tests live in `test/toon-mode-tests.el` and run via `make test`.
- Validate changes manually:
  - Load the mode in Emacs.
  - Confirm `.toon` file indentation updates via `TAB`.
  - Confirm syntax highlighting for headers, keys, and constants.

## Commit & Pull Request Guidelines

- There is no established commit-message convention in this repo; use short, imperative summaries (e.g., "Fix indentation for array items").
