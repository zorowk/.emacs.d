# Test findings

This file records configuration issues exposed by automated validation.

## F003 — AUCTeX interfaces are unknown during isolated compilation

- Status: accepted deferred-package warning
- Severity: low
- Evidence: warning-as-error byte compilation of `elisp/init-latex.el` cannot
  see AUCTeX and RefTeX declarations before those deferred packages load.
- Decision: keep the working runtime configuration unchanged rather than add
  declarations solely for the test compiler.

## Resolved in the compatibility follow-up

- Denote search has one key owner: `consult-denote-grep`.
- Crux remains available for its editing commands, but its three legacy advice
  macros and duplicate `crux-eval-and-replace` key are no longer configured.
- Hyperbole follows the official Savannah repository through Emacs 31
  `use-package :vc`, providing the 9.0.2pre HyWiki API.
