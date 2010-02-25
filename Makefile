##                         Copyright (C) 2010, Sogilis                       ##

GNATMAKE=gnatmake
TEST_SUITES=test coverage
CONFIG=debug

all: bin test doc
	@echo
	@echo "####################################################"
	@echo "##                                                ##"
	@echo "##  Run 'make help' to get help on this Makefile  ##"
	@echo "##                                                ##"
	@echo "####################################################"

dir:
	mkdir -p lib
	mkdir -p obj
	mkdir -p bin
	mkdir -p doc
	mkdir -p reports

bin: dir
	$(GNATMAKE) -P $(CONFIG).gpr

test: dir
	$(GNATMAKE) -P tests.gpr

doc: dir README.html src/README.html
	
clean:
	-$(RM) -rf tmp
	-$(RM) obj/*
	-$(RM) bin/*
	-$(RM) README.html
	-$(RM) src/README.html
	-$(RM) reports/*.gcov
	-$(RM) reports/gcov.summary.txt
	-$(RM) reports/gnatcheck.out
	-$(RM) reports/gnatcheck.log
	-$(RM) reports/gnatcheck.*.out
	-$(RM) reports/gnatcheck.*.log
	-$(RM) reports/*.aunit.gcov
	-$(RM) reports/features.html
	-$(RM) reports/features.junit/*

gcov-reset: dir
	-$(RM) reports/*.gcov
	-$(RM) reports/gcov.summary.txt
	-$(RM) obj/*.gcda

gcov-prepare: bin
	cd reports && gcov -o ../obj ../src/*.adb > gcov.summary.txt
	for gcov in reports/*.gcov; do \
		base="`basename "$$gcov"`"; \
		if [ ! -e "src/$${base%.gcov}" ]; then \
			rm "$$gcov"; \
		fi; \
	done

gcov:
	@$(MAKE) gcov-prepare >/dev/null 2>&1
	bin/tests -suite=coverage -text

coverage: test bin
	$(MAKE) gcov-reset
	-bin/tests >/dev/null 2>/dev/null
	-cucumber features/*.feature >/dev/null 2>/dev/null
	$(MAKE) gcov

gnatcheck: dir
	cd reports && gnat check -P ../$(CONFIG).gpr -rules -from=../gnatcheck.rules
	cd reports && mv gnatcheck.out gnatcheck.main.out
	cd reports && gnat check -P ../tests.gpr -rules -from=../gnatcheck.rules
	cd reports && mv gnatcheck.out gnatcheck.tests.out

test-report: dir bin test
	for t in $(TEST_SUITES); do \
	  echo "========== RUN TEST SUITE $$t =========="; \
	  echo bin/tests -xml -suite="$$t" -o"reports/$$t.aunit.xml"; \
	  bin/tests -xml -suite="$$t" -o"reports/$$t.aunit.xml"; \
	  cat "reports/$$t.aunit.xml"; \
	done
	-mkdir -p reports/features.junit
	-cucumber -f junit -o reports/features.junit features/*.feature
	-cucumber -f html -o reports/features.html features/*.feature

run-cucumber-tests: bin
	cucumber features/*.feature
	

run-tests: dir bin test
	$(MAKE) gcov-reset
	-$(MAKE) run-cucumber-tests
	@for t in $(TEST_SUITES); do \
	  [ coverage = "$$t" ] && continue; \
	  echo "========== RUN TEST SUITE $$t =========="; \
	  echo bin/tests -suite="$$t"; \
	  bin/tests -suite="$$t"; \
	done
	@echo "========== RUN COVERAGE TESTS =========="
	$(MAKE) gcov

clean-reports: gcov-reset
	-$(RM) reports/gnatcheck.out
	-$(RM) reports/*.aunit.gcov

check: bin clean-reports coverage gnatcheck
	for t in $(TEST_SUITES); do \
	  echo bin/tests -suite="$$t"; \
	  bin/tests -suite="$$t"; \
	done

show-ignored-coverage:
	find src -name "*.ad[bs]" -print0 | xargs -0 grep -Rn GCOV_IGNORE

.PHONY: all dir bin test doc clean clean-reports gcov-reset gcov-prepare gcov \
        coverage gnatcheck check test-report run-tests run-cucumber-tests \
        show-ignored-coverage help

help:
	@echo "make TARGET"
	@echo
	@echo "Targets:"
	@echo
	@echo "    all:            Build everything    [bin tests doc]"
	@echo "    bin:            Build project       [bin/adaspec]"
	@echo "    tests:          Build tests         [bin/tests]"
	@echo "    doc:            Build documentation [README.html]"
	@echo "    coverage:       Run coverage tests  [reports/*.gcov]"
	@echo "    gnatcheck:      Run gnatcheck       [reports/gnatcheck.*]"
	@echo "    test-report:    Create test reports [reports/*.aunit.xml]"
	@echo "    run-tests:      Run all tests"
	@echo "    show-ignored-coverage:"
	@echo "                    Show lines that are ignored by gcov"
	@echo "    clean:          Clean project"



### Markdown ###

MARKDOWN_URL=http://daringfireball.net/projects/downloads/Markdown_1.0.1.zip
MARKDOWN_DIR=Markdown_1.0.1
MARKDOWN_CMDLINE=./Markdown.pl <$< >$@

Markdown.zip:
	wget $(MARKDOWN_URL) -O $@

Markdown.pl:
	$(MAKE) Markdown.zip
	unzip -u -j Markdown.zip $(MARKDOWN_DIR)/$@
	chmod +x $@
	-$(RM) Markdown.zip

%.html: %.mdwn Markdown.pl
	$(MARKDOWN_CMDLINE)

%.html: % Markdown.pl
	$(MARKDOWN_CMDLINE)
