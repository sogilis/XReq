##                         Copyright (C) 2010, Sogilis                       ##

GNATMAKE=gnatmake
TEST_SUITES=test coverage
CONFIG=dbg

GPRBUILD=gprbuild
GPS=gps
INSTALL=install
CP=cp -R

DESTDIR    =
PREFIX     =$(shell which $(GPRBUILD) 2>/dev/null | sed -e's/\/bin\/[^/]*//')
PREFIX_GPS =$(shell which $(GPS) 2>/dev/null | sed -e's/\/bin\/[^/]*//')
ifeq ($(PREFIX),)
PREFIX     =/usr/local
endif
BINDIR     = $(PREFIX)/bin
INCLUDEDIR = $(PREFIX)/include
LIBDIR     = $(PREFIX)/lib
GPRDIR     = $(PREFIX)/lib/gnat
DATADIR    = $(PREFIX)/share
GPSDATADIR = $(PREFIX_GPS)/share/gps
DOCDIR     = $(DATADIR)/doc/AdaSpec

all: bin tests doc
	@echo
	@echo "####################################################"
	@echo "##                                                ##"
	@echo "##  Run 'make help' to get help on this Makefile  ##"
	@echo "##                                                ##"
	@echo "####################################################"

dir:
	mkdir -p src/common
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
	$(GPRBUILD) -Padaspec.gpr -Xmode=coverage

bin/adaspec.rel: dir
	$(GPRBUILD) -Padaspec.gpr -Xmode=release

bin/adaspec.dbg: dir
	$(GPRBUILD) -Padaspec.gpr -Xmode=debug

bin/adaspec: bin/adaspec.$(CONFIG)
	-rm -f bin/adaspec
	ln -s adaspec.$(CONFIG) bin/adaspec

bin/unit_tests: dir
	$(GPRBUILD) -Punit_tests.gpr -Xmode=debug

bin/unit_tests.cov: dir
	$(GPRBUILD) -Punit_tests.gpr -Xmode=coverage

bootstrap: bin/feature_tests

bin/feature_tests: bin/feature_tests.dbg
	ln -s feature_tests.dbg $@

bin/feature_tests.dbg: bin/adaspec features/*.feature
	-$(MKDIR) features/tests/dbg
	GNAT_FLAGS="-g -gnata -E" \
	bin/adaspec -m -x feature_tests -o features/tests/dbg features/*.feature
	$(CP) features/tests/dbg/feature_tests $@

bin/feature_tests.cov: bin/adaspec features/*.feature
	-$(MKDIR) features/tests/cov
	GNAT_FLAGS="-g -gnata -E -fprofile-arcs -ftest-coverage" \
	bin/adaspec -m -x feature_tests -o features/tests/cov features/*.feature
	$(CP) features/tests/cov/feature_tests $@

tests: dir
	$(GPRBUILD) -Punit_tests.gpr

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

.PHONY: all dir bin tests bootstrap doc clean



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
	lcov --remove coverage/lcov.info '*__*'   -o coverage/lcov.info
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
	$(GPRBUILD) -P adaspec.gpr -Xmode=coverage
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
#	-\
#	GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
#	COVERAGE="`pwd`/coverage/cuke" COV_OBJ_DIR="`pwd`/obj/coverage" mode=coverage \
#	cucumber -t "@bootstrap" features/*.feature
	-\
	GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
	COVERAGE="`pwd`/coverage/cuke" COV_OBJ_DIR="`pwd`/obj/coverage" mode=coverage \
	bin/adaspec.cov -m -x bootstrap_suite features/*.feature ; \
	features/tests/bootstrap_suite -t ~@bootstrap -f html -o reports/features-adaspec.html
#	-\
#	GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
#	COVERAGE="`pwd`/coverage/cuke" COV_OBJ_DIR="`pwd`/obj/coverage" mode=coverage \
#	cucumber -t "~@wip" -t "~@bootstrap" -f html  -o reports/features.html      features/*.feature >/dev/null 2>&1
#	-\
#	GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
#	COVERAGE="`pwd`/coverage/cuke" COV_OBJ_DIR="`pwd`/obj/coverage" mode=coverage \
#	cucumber -w -t "@wip" -f html  -o reports/features-wip.html  features/*.feature >/dev/null 2>&1
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
	$(GPRBUILD) -Pfeatures/tests/suite.gpr
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
	cd reports && gnat check -P../adaspec.gpr -U -rules -from=../gnatcheck.rules
	cd reports && mv gnatcheck.out gnatcheck.adaspec.out
	#cd reports && gnat check -P../adaspeclib.gpr -U -rules -from=../gnatcheck.rules
	#cd reports && mv gnatcheck.out gnatcheck.adaspeclib.out
	#cd reports && gnat check -P../unit_tests.gpr -rules -from=../gnatcheck.rules
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
	$(GPRBUILD) -Pfeatures/tests/suite.gpr

reports/features-adaspec.html: features/tests/suite
	-features/tests/suite -f html -o reports/features-adaspec.html

check: gnatcheck coverage run-cucumber run-tests

.PHONY: gnatcheck test-report test-report-cucumber test-report-unit check

install: bin/adaspec.rel
	$(INSTALL) -D bin/adaspec.rel $(DESTDIR)$(BINDIR)/adaspec
	$(INSTALL) -m644 -D data/adaspeclib.gpr $(DESTDIR)$(GPRDIR)/adaspeclib.gpr
	$(INSTALL) -d $(DESTDIR)$(INCLUDEDIR)/adaspeclib
	$(CP) src/lib/*.ad[bs] $(DESTDIR)$(INCLUDEDIR)/adaspeclib
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/adaspeclib
	$(CP) lib/release/* $(DESTDIR)$(LIBDIR)/adaspeclib
	$(INSTALL) -m644 data/gpr-plug-in/adaspec.xml      $(DESTDIR)$(GPSDATADIR)/plug-ins/adaspec.xml
	$(INSTALL) -m644 data/gpr-plug-in/adaspec.py       $(DESTDIR)$(GPSDATADIR)/plug-ins/adaspec.py
	$(INSTALL) -m644 data/gpr-plug-in/feature-lang.xml $(DESTDIR)$(GPSDATADIR)/plug-ins/feature-lang.xml
	@echo '------------------------------------------------------------------'
	@echo '--  AdaSpec has now been installed.'
	@echo '------------------------------------------------------------------'
	@echo '--  To be able to use the adaspec binary, you may need to update'
	@echo '--  your PATH to point to'
	@echo '--  $(DESTDIR)$(BINDIR)'
	@echo '------------------------------------------------------------------'
	@echo '--  To be able to use the library, you may need to update your'
	@echo '--  ADA_PROJECT_PATH or GPR_PROJECT_PATH to point to the path'
	@echo '--  $(DESTDIR)$(GPRDIR)'
	@echo '------------------------------------------------------------------'

uninstall:
	-$(RM) -rf $(DESTDIR)$(BINDIR)/adaspec
	-$(RM) -rf $(DESTDIR)$(GPRDIR)/adaspeclib.gpr
	-$(RM) -rf $(DESTDIR)$(INCLUDEDIR)/adaspeclib
	-$(RM) -rf $(DESTDIR)$(LIBDIR)/adaspeclib
	-$(RM) -rf $(DESTDIR)$(DOCDIR)
	-$(RM) -rf $(DESTDIR)$(DATADIR)/AdaSpec
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/plug-ins/adaspec.xml
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/plug-ins/adaspec.py
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/plug-ins/feature-lang.xml

install-gps-local:
	ln -sf "`pwd`"/data/gpr-plug-in/*.{xml,py} ~/.gps/plug-ins

.PHONY: install uninstall install-gps-local

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
	@echo "    install:        Install AdaSpec"
	@echo "    unnstall:       Uninstall AdaSpec"
	@echo "    install-gps-local:"
	@echo "                    Install the GPS plugin in your HOME directory"
	@echo "                    using symbolic links rather than copying files"
	@echo "    show-ignored-coverage:"
	@echo "                    Show lines that are ignored by gcov"
	@echo
	@echo "Variables:"
	@echo
	@echo "    DESTDIR         root for all installation [$(DESTDIR)]"
	@echo "    PREFIX          prefix for installation   [$(PREFIX)]"
	@echo "    PREFIX_GPS      prefix for GPS plugin     [$(PREFIX_GPS)]"
	@echo "    BINDIR          user executables          [$(BINDIR)]"
	@echo "    INCLUDEDIR      ???                       [$(INCLUDEDIR)]"
	@echo "    LIBDIR          ???                       [$(LIBDIR)]"
	@echo "    GPRDIR          GNAT Project files        [$(GPRDIR)]"
	@echo "    DOCDIR          Documentation directory   [$(DOCDIR)]"
	@echo "    DATADIR         Read only architecture-independant data"
	@echo "                                              [$(DATADIR)]"
	@echo "    GPSDATADIR      GPS plugin data           [$(GPSDATADIR)]"

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
