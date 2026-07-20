;;; run-tests.el --- Batch entry point for configuration tests -*- lexical-binding: t; -*-

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'test-helper)
(require 'init-functions-tests)
(require 'integration-tests)

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
