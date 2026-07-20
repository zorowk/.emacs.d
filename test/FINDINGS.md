# Test findings

This file records configuration issues exposed by automated validation.

There are currently no open findings.

## Resolved in the compatibility follow-up

- Denote search has one key owner: `consult-denote-grep`.
- Crux remains available for its editing commands, but its three legacy advice
  macros and duplicate `crux-eval-and-replace` key are no longer configured.
- The compiler loads AUCTeX and RefTeX declarations before checking the
  unchanged deferred LaTeX configuration with warnings as errors.
- Hyperbole follows the official Savannah repository through Emacs 31
  `use-package :vc`, providing the 9.0.2pre HyWiki API.
