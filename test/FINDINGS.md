# Test findings

This file records configuration issues exposed by automated validation.

## F002 — Crux macros expand obsolete advice on Emacs 31

- Status: accepted upstream warning
- Severity: low
- Evidence: warning-as-error byte compilation of `elisp/init-edit.el` reports
  three uses of the obsolete `defadvice` macro.  They originate in Crux 0.5.0's
  documented `crux-with-region-*` macros, not in local functions.
- Decision: keep the package's documented configuration unchanged.

## F003 — AUCTeX interfaces are unknown during isolated compilation

- Status: accepted deferred-package warning
- Severity: low
- Evidence: warning-as-error byte compilation of `elisp/init-latex.el` cannot
  see AUCTeX and RefTeX declarations before those deferred packages load.
- Decision: keep the working runtime configuration unchanged rather than add
  declarations solely for the test compiler.

## Resolved in the compatibility follow-up

- Denote search has one key owner: `consult-denote-grep`.
- Hyperbole follows the official Savannah repository through Emacs 31
  `use-package :vc`, providing the 9.0.2pre HyWiki API.
