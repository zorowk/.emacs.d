# Test findings

This file records configuration issues exposed while building the test suite.
The test-infrastructure change intentionally does not fix them; each finding
should be resolved and removed in a follow-up change with its regression test.

## F001 — Denote search key has two owners

- Status: confirmed
- Severity: medium
- Evidence: `elisp/init-org.el` binds `C-c n g` to both `denote-grep` and
  `consult-denote-grep`.  After startup the latter wins.
- Test: `zoro-integration-denote-search-key-has-single-declaration` is marked as
  an expected failure until one owner is selected.

## F002 — Crux macros expand obsolete advice on Emacs 31

- Status: confirmed with Crux 0.5.0 and Emacs 31.0.90
- Severity: medium
- Evidence: warning-as-error byte compilation of `elisp/init-edit.el` reports
  three uses of the obsolete `defadvice` macro.  They originate in
  `crux-with-region-or-buffer` and `crux-with-region-or-point-to-eol`.
- Impact: normal startup succeeds, but strict compatibility compilation fails.

## F003 — AUCTeX variable is not declared for compilation

- Status: confirmed with AUCTeX 14.1.2 and Emacs 31.0.90
- Severity: low
- Evidence: warning-as-error byte compilation of `elisp/init-latex.el` reports
  assignment to the free variable `TeX-PDF-mode`.
- Impact: normal startup succeeds, but strict compatibility compilation fails.

## F004 — Hyperbole compilation depends on writable user state

- Status: needs confirmation on a clean GitHub runner
- Severity: medium
- Evidence: compiling `elisp/init-hyperbole.el` caused Hyperbole 9.0.1 to
  initialize before compilation completed; initialization rejected a
  non-writable `hbmap:dir-user`, after which `hywiki-mode` was unknown to the
  compiler.
- Uncertainty: the local managed sandbox cannot write the Dropbox HyWiki path,
  so part of this result may be environment-specific.  The clean CI job will
  provide decisive evidence.
