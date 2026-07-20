# Test findings

This file records configuration issues exposed by automated validation.

There are currently no open findings.

## Resolved in the compatibility follow-up

- Denote search has one key owner: `consult-denote-grep`.
- Crux keeps its documented region macros; only their upstream obsolete-advice
  compiler warning is suppressed locally.
- AUCTeX and RefTeX external variables and callback are declared for the
  compiler without changing runtime behavior.
- Hyperbole follows the official Savannah repository through Emacs 31
  `use-package :vc`, providing the 9.0.2pre HyWiki API.
