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
	mkdir -p lib/release
	mkdir -p lib/debug
	mkdir -p lib/coverage
	mkdir -p obj
	mkdir -p obj/release
	mkdir -p obj/debug
	mkdir -p obj/coverage
	mkdir -p bin
	mkdir -p doc
	mkdir -p reports
	mkdir -p coverage

bin: dir
	$(GNATMAKE) -P adaspec-$(CONFIG).gpr

test: dir
	$(GNATMAKE) -P unit_tests.gpr

doc: dir README.html src/README.html
	
clean: clean-gcov
	-$(RM) -rf tmp
	-$(RM) -rf obj/*
	-$(RM) -rf lib/*
	-$(RM) bin/*
	-$(RM) README.html
	-$(RM) src/README.html
	-$(RM) reports/*.aunit.xml
	-$(RM) reports/gnatcheck*.out
	-$(RM) reports/gnatcheck*.log
# 	-$(RM) reports/*.aunit.gcov
# 	-$(RM) reports/features.html
# 	-$(RM) reports/features.junit/*

.PHONY: all dir bin test doc clean



clean-gcov:
	-$(RM) coverage/*.gcov
	-$(RM) coverage/gcov.summary.txt
	-find obj -name "*.gcda" -print0 | xargs -0 rm -f

gcov-report: dir
	cd coverage && gcov -o ../obj/coverage ../src/*.adb ../src/lib/*.adb > gcov.summary.txt
	for gcov in coverage/*.gcov; do \
		base="`basename "$$gcov"`"; \
		if [ ! -e "src/$${base%.gcov}" ] && [ ! -e "src/lib/$${base%.gcov}" ]; then \
			rm "$$gcov"; \
		fi; \
	done
	-bin/unit_tests -suite=coverage -text
	bin/unit_tests -suite=coverage -xml -o reports/coverage.aunit.xml

coverage: test
	@echo
	@echo "##########################"
	@echo "##  Run coverage tests  ##"
	@echo "##########################"
	@echo
	gnatmake -P adaspec-coverage.gpr
	-$(RM) -f bin/adaspec
	cp bin/adaspec.cov bin/adaspec
	$(MAKE) clean-gcov
	-bin/unit_tests >/dev/null 2>&1
	-$(MAKE) run-cucumber >/dev/null 2>&1
	$(MAKE) gcov-report
	-$(RM) -f bin/adaspec

.PHONY: clean-gcov gcov-report coverage


run-cucumber: bin
	@echo
	@echo "####################"
	@echo "##  Run cucumber  ##"
	@echo "####################"
	@echo
	cucumber -t "~@wip" features/*.feature
	cucumber -w -t "@wip" features/*.feature

run-tests: test
	@echo
	@echo "######################"
	@echo "##  Run unit tests  ##"
	@echo "######################"
	@echo
	bin/unit_tests

.PHONY: run-cucumber run-tests

gnatcheck: dir
	@echo
	@echo "######################"
	@echo "##  Run GNAT-Check  ##"
	@echo "######################"
	@echo
	cd reports && gnat check -P ../adaspec.gpr -rules -from=../gnatcheck.rules
	cd reports && mv gnatcheck.out gnatcheck.adaspec.out
	cd reports && gnat check -P ../adaspeclib.gpr -rules -from=../gnatcheck.rules
	cd reports && mv gnatcheck.out gnatcheck.adaspeclib.out
	cd reports && gnat check -P ../unit_tests.gpr -rules -from=../gnatcheck.rules
	cd reports && mv gnatcheck.out gnatcheck.tests.out

test-report:
	@echo
	@echo "##################################"
	@echo "##  Generate unit test reports  ##"
	@echo "##################################"
	@echo
	-$(MAKE) coverage
	-bin/unit_tests -o reports/test.aunit.xml
	-mkdir -p reports/features.junit
	-mkdir -p reports/features-wip.junit
	@echo
	@echo "######################################"
	@echo "##  Generate cucumber test reports  ##"
	@echo "######################################"
	@echo
	-cucumber -t "~@wip"   -f junit -o reports/features.junit     features/*.feature
	-cucumber -t "~@wip"   -f html  -o reports/features.html      features/*.feature
	-cucumber -w -t "@wip" -f junit -o reports/features-wip.junit features/*.feature
	-cucumber -w -t "@wip" -f html  -o reports/features-wip.html  features/*.feature

check: gnatcheck coverage run-cucumber run-tests

.PHONY: gnatcheck test-report check

show-ignored-coverage:
	find src -name "*.ad[bs]" -print0 | xargs -0 grep -Rn GCOV_IGNORE

help:
	@echo "make TARGET"
	@echo
	@echo "Targets:"
	@echo
	@echo "    all:            Build everything    [bin tests doc]"
	@echo "    bin:            Build project       [bin/adaspec]"
	@echo "    test:           Build tests         [bin/unit_tests]"
	@echo "    doc:            Build documentation [README.html]"
	@echo "    coverage:       Run coverage tests  [coverage/]"
	@echo "    gnatcheck:      Run gnatcheck       [reports/gnatcheck*]"
	@echo "    test-report:    Create test reports [reports/]"
	@echo "    run-tests:      Run all unit tests"
	@echo "    run-cucumber:   Run all cucumber tests"
	@echo "    clean:          Clean project"
	@echo "    show-ignored-coverage:"
	@echo "                    Show lines that are ignored by gcov"

.PHONY: help show-ignored-coverage

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
