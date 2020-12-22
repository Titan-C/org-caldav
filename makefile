test: cal-sync.el cal-sync-test.el
	emacs --batch -Q -L . -l cal-sync-test.el -f ert-run-tests-batch-and-exit
