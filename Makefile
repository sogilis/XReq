##                         Copyright (C) 2010, Sogilis                       ##

GNATMAKE=gnatmake
TEST_SUITES=test coverage
CONFIG=dbg

all: bin tests doc
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

bin: bin/adaspec

bin/adaspec.cov: dir
	$(GNATMAKE) -P adaspec-coverage.gpr

bin/adaspec.rel: dir
	$(GNATMAKE) -P adaspec-release.gpr

bin/adaspec.dbg: dir
	$(GNATMAKE) -P adaspec-debug.gpr

bin/adaspec: bin/adaspec.$(CONFIG)
	-rm -f bin/adaspec
	ln -s adaspec.$(CONFIG) bin/adaspec

bin/unit_tests: dir
	$(GNATMAKE) -P unit_tests-debug.gpr

bin/unit_tests.cov: dir
	$(GNATMAKE) -P unit_tests-coverage.gpr

tests: dir
	$(GNATMAKE) -P unit_tests-debug.gpr

doc: dir README.html src/README.html reports/index.html
	
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
	-$(RM) reports/*.gcov
	-$(RM) reports/features*.html
	-$(RM) reports/features*.junit/*

.PHONY: all dir bin tests doc clean



clean-gcov:
	-$(RM) -rf coverage/lcov.info coverage/*.lcov.info coverage/*
	-lcov -q -d obj/coverage --zerocounters
	

gcov-report: dir bin/unit_tests
	@echo
	@echo "###############################"
	@echo "##  Create coverage reports  ##"
	@echo "###############################"
	@echo
	lcov -q -c -i -d obj/coverage -t "Base" -o coverage/1-base.lcov.info
	lcov -q $(foreach lcov,$(wildcard coverage/*.lcov.info),-a $(lcov)) -o coverage/lcov.info
	lcov --remove coverage/lcov.info '/opt/*' -o coverage/lcov.info
	lcov --remove coverage/lcov.info '/usr/*' -o coverage/lcov.info
	lcov --remove coverage/lcov.info '*~*'    -o coverage/lcov.info
	#perl gcov-ignore.pl coverage/lcov.info > coverage/ignore.lcov.info
	genhtml --show-details --no-function-coverage -o coverage coverage/lcov.info || \
	genhtml --show-details -o coverage coverage/lcov.info
	#cd coverage && gcov -o ../obj/coverage ../src/*.adb ../src/lib/*.adb > gcov.summary.txt
	#for gcov in coverage/*.gcov; do \
	#	base="`basename "$$gcov"`"; \
	#	if [ ! -e "src/$${base%.gcov}" ] && [ ! -e "src/lib/$${base%.gcov}" ]; then \
	#		rm "$$gcov"; \
	#	fi; \
	#done
	lcov -d obj/coverage --zerocounters
	-bin/unit_tests -suite=coverage -text
	bin/unit_tests -suite=coverage -xml -o reports/coverage.aunit.xml

coverage: tests bin/adaspec.cov bin/unit_tests.cov
	@echo
	@echo "##########################"
	@echo "##  Run coverage tests  ##"
	@echo "##########################"
	@echo
	gnatmake -P adaspec-coverage.gpr
	-$(RM) -f bin/adaspec
	$(MAKE) clean-gcov
	lcov -q -c -i -d obj/coverage -t "Ignored_Lines" -o coverage/2-ignore.lcov.info.tmp
	perl gcov-ignore.pl coverage/2-ignore.lcov.info.tmp > coverage/2-ignore.lcov.info
	ln -s adaspec.cov bin/adaspec
	@echo
	@echo "==>  Run Unit tests"
	@echo
	lcov -q -d obj/coverage --zerocounters
	-bin/unit_tests.cov -text -o reports/test.aunit.txt > reports/test-debug.aunit.txt 2>&1
	lcov -q -c -d obj/coverage -t "Unit_Tests" -o coverage/10-unit.lcov.info
	@echo
	@echo "==>  Run Cucumber"
	@echo
	-rm -rf coverage/cuke
	-mkdir -p coverage/cuke
	lcov -q -d obj/coverage --zerocounters
	-\
	GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
	COVERAGE="`pwd`/coverage/cuke" COV_OBJ_DIR="`pwd`/obj/coverage" \
	cucumber -t "~@wip"   -f html  -o reports/features.html      features/*.feature >/dev/null 2>&1
	-\
	GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
	COVERAGE="`pwd`/coverage/cuke" COV_OBJ_DIR="`pwd`/obj/coverage" \
	cucumber -w -t "@wip" -f html  -o reports/features-wip.html  features/*.feature >/dev/null 2>&1
	lcov -q -c -d obj/coverage -t "Cucumber" -o coverage/cuke/last.lcov.info
	$(MAKE) _gcov-gather-cucumber
	-$(RM) -f bin/adaspec
	$(MAKE) gcov-report
	ln -s adaspec.$(CONFIG) bin/adaspec

_gcov-gather-cucumber:
	lcov $(foreach lcov,$(wildcard coverage/cuke/*.lcov.info),-a $(lcov)) -o coverage/11-cucumber.lcov.info
	-rm -rf coverage/cuke

.PHONY: clean-gcov gcov-report _gcov-gather-cucumber coverage


run-wip-cucumber: bin
	-$(RM) -f cucumber-rerun.txt
	cucumber -w -t "@wip" -f progress -f rerun -o cucumber-rerun.txt features/*.feature

run-cucumber: bin
	@echo
	@echo "####################"
	@echo "##  Run cucumber  ##"
	@echo "####################"
	@echo
	@$(MAKE) run-wip-cucumber
	-$(RM) -f cucumber-rerun.txt
	cucumber -t "~@wip" -t "~@bootstrap" -f progress -f rerun -o cucumber-rerun.txt features/*.feature

run-bootstrap: bin
	cucumber -t "~@wip" -t "@bootstrap" features/*.feature

rerun-cucumber: bin
	@echo
	@echo "##########################################"
	@echo "##  Run cucumber scenarios that failed  ##"
	@echo "##########################################"
	@echo
	touch cucumber-rerun.txt
	@if [ -s cucumber-rerun.txt ]; then \
		xargs echo cucumber < cucumber-rerun.txt; \
		xargs cucumber < cucumber-rerun.txt; \
	fi
	-$(RM) -f cucumber-rerun.txt

run-adaspec: bin
	@echo
	@echo "###################"
	@echo "##  Run AdaSpec  ##"
	@echo "###################"
	@echo
	bin/adaspec -x suite features/*.feature
	gnatmake -P features/tests/suite.gpr
	features/tests/suite

run-tests: tests bin
	@echo
	@echo "######################"
	@echo "##  Run unit tests  ##"
	@echo "######################"
	@echo
	bin/unit_tests

.PHONY: run-cucumber rerun-cucumber run-wip-cucumber run-tests

gnatcheck: dir
	@echo
	@echo "######################"
	@echo "##  Run GNAT-Check  ##"
	@echo "######################"
	@echo
	cd reports && gnat check -P ../adaspec-release.gpr -U -rules -from=../gnatcheck.rules
	cd reports && mv gnatcheck.out gnatcheck.adaspec.out
	#cd reports && gnat check -P ../adaspeclib-release.gpr -U -rules -from=../gnatcheck.rules
	#cd reports && mv gnatcheck.out gnatcheck.adaspeclib.out
	#cd reports && gnat check -P ../unit_tests-debug.gpr -rules -from=../gnatcheck.rules
	#cd reports && mv gnatcheck.out gnatcheck.tests.out

test-report:
	-$(MAKE) coverage
	-$(MAKE) test-report-unit
	-$(MAKE) test-report-cucumber
	-#$(MAKE) test-report-adaspec # Not necessary, bootstrap.feature does it

test-report-unit: tests bin
	@echo
	@echo "##################################"
	@echo "##  Generate unit test reports  ##"
	@echo "##################################"
	@echo
	-bin/unit_tests -xml -o reports/test.aunit.xml
	-bin/unit_tests -text -o reports/test.aunit.txt > reports/test-debug.aunit.txt 2>&1

test-report-cucumber: bin
	@echo
	@echo "######################################"
	@echo "##  Generate cucumber test reports  ##"
	@echo "######################################"
	@echo
	-mkdir -p reports/features.junit
	-mkdir -p reports/features-wip.junit
	-cucumber -t "~@wip"   -f junit -o reports/features.junit     features/*.feature
	-cucumber -t "~@wip"   -f html  -o reports/features.html      features/*.feature
	-cucumber -w -t "@wip" -f junit -o reports/features-wip.junit features/*.feature
	-cucumber -w -t "@wip" -f html  -o reports/features-wip.html  features/*.feature

test-report-adaspec: bin reports/features-adaspec.html

features/tests/suite.gpr: bin/adaspec $(wildcard features/*.feature)
	bin/adaspec -x suite $(wildcard features/*.feature)

features/tests/suite: features/tests/suite.gpr
	gnatmake -P features/tests/suite.gpr

reports/features-adaspec.html: features/tests/suite
	-features/tests/suite -f html -o reports/features-adaspec.html

check: gnatcheck coverage run-cucumber run-tests

.PHONY: gnatcheck test-report test-report-cucumber test-report-unit check

show-ignored-coverage:
	find src -name "*.ad[bs]" -print0 | xargs -0 grep -Rn GCOV_IGNORE

help:
	@echo "make TARGET"
	@echo
	@echo "Targets:"
	@echo
	@echo "    all:            Build everything    [bin tests doc]"
	@echo "    bin:            Build project       [bin/adaspec]"
	@echo "    tests:          Build tests         [bin/unit_tests]"
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

src/lib/adaspeclib-format_html_template.ads src/lib/adaspeclib-format_html_template.adb: src/adaspec-report.template.html ./template.pl
	./template.pl $< AdaSpecLib.Format_HTML_Template $@

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
