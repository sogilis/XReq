##                         Copyright (C) 2010, Sogilis                       ##

GNATMAKE=gnatmake
TEST_SUITES=test coverage
CONFIG=dbg
INSTALL_CONFIG=rel
MODE=
LIBTYPE=static
# dynamic makes gcov unhappy: hidden symbol `__gcov_merge_add' is referenced by
# DSO (Dynamic Shared Object).
LIBEXT=

ifeq ($(INSTALL_CONFIG),dbg)
INSTALL_MODE=debug
endif
ifeq ($(INSTALL_CONFIG),rel)
INSTALL_MODE=release
endif
ifeq ($(INSTALL_CONFIG),cov)
INSTALL_MODE=coverage
endif

ifeq ($(CONFIG),dbg)
MODE=debug
endif
ifeq ($(CONFIG),rel)
MODE=release
endif
ifeq ($(CONFIG),cov)
MODE=coverage
endif
ifeq ($(LIBTYPE),static)
LIBEXT=a
endif
ifeq ($(LIBTYPE),dynamic)
LIBEXT=so
endif

VERBOSE=

GPRBUILD=gprbuild
GPS=gps
INSTALL=install
CP=cp -R
MKDIR=mkdir -p

ifeq ($(VERBOSE),)
GPRBUILD_FLAGS=-p -q
XREQ_FLAGS=-q
else
GPRBUILD_FLAGS=-p -v
XREQ_FLAGS=
endif

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
DOCDIR     = $(DATADIR)/doc/XReq

all: bin lib gps-plugin tests doc
	@echo
	@echo "####################################################"
	@echo "##                                                ##"
	@echo "##  Run 'make help' to get help on this Makefile  ##"
	@echo "##                                                ##"
	@echo "####################################################"



check-all: all gnatcheck run-unit run-cucumber run-xreq coverage

$(VERBOSE).SILENT:

########################
##                    ##
##    MAIN TARGETS    ##
##                    ##
########################

dir:
	@-mkdir -p src/lib/static
	@-mkdir -p src/lib/dynamic
	@-mkdir -p lib
	@-mkdir -p lib/gps
	@-mkdir -p lib/release
	@-mkdir -p lib/debug
	@-mkdir -p lib/coverage
	@-mkdir -p obj
	@-mkdir -p obj/gps
	@-mkdir -p obj/release
	@-mkdir -p obj/debug
	@-mkdir -p obj/coverage
	@-mkdir -p bin
	@-mkdir -p doc
	@-mkdir -p reports
	@-mkdir -p coverage

bin: bin/xreq

lib: lib/$(MODE)/libxreqlib.$(LIBEXT) lib/$(MODE)/libxreq.so

lib/debug/libxreq.so: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Plibxreq.gpr -Xtype=dynamic -Xmode=debug

lib/release/libxreq.so: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Plibxreq.gpr -Xtype=dynamic -Xmode=release

lib/coverage/libxreq.so: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Plibxreq.gpr -Xtype=dynamic -Xmode=coverage

lib/debug/libxreqlib.so: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=dynamic -Xmode=debug

lib/release/libxreqlib.so: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=dynamic -Xmode=release

lib/coverage/libxreqlib.so: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=dynamic -Xmode=coverage

lib/debug/libxreqlib.a: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=static  -Xmode=debug

lib/release/libxreqlib.a: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=static  -Xmode=release

lib/coverage/libxreqlib.a: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=static  -Xmode=coverage

bin/xreq.cov: lib/coverage/libxreqlib.$(LIBEXT)
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreq.gpr    -Xtype=$(LIBTYPE) -Xmode=coverage

bin/xreq.rel: lib/release/libxreqlib.$(LIBEXT)
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreq.gpr    -Xtype=$(LIBTYPE) -Xmode=release

bin/xreq.dbg: lib/debug/libxreqlib.$(LIBEXT)
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreq.gpr    -Xtype=$(LIBTYPE) -Xmode=debug

bin/xreq: bin/xreq.$(CONFIG)
	@echo "LINK    $@"
	-rm -f bin/xreq
	ln -s xreq.$(CONFIG) bin/xreq

lib/gps/libxreqgps.so: dir
	@echo "LINK    $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pgps_plugin.gpr -Xmode=release
	#$(MAKE) -C src/gps libgprcustom.so && mv src/gps/libgprcustom.so $@

gps-plugin: lib/gps/libxreqgps.so

bin/unit_tests.dbg: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Punit_tests.gpr -Xmode=debug

bin/unit_tests.cov: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Punit_tests.gpr -Xmode=coverage

bin/feature_tests: bin/feature_tests.dbg
	@echo "LINK    $@"
	ln -s -f feature_tests.dbg $@

bin/feature_tests.dbg: bin/xreq features/*.feature
	@echo "XREQ    $@"
	-$(MKDIR) features/tests/dbg
	GNAT_FLAGS="-g -gnata -E" \
	bin/xreq $(XREQ_FLAGS) -m -x feature_tests -o features/tests/dbg features/*.feature
	$(CP) features/tests/dbg/feature_tests $@

bin/feature_tests.cov: bin/xreq features/*.feature
	@echo "XREQ    $@"
	-$(MKDIR) features/tests/cov
	GNAT_FLAGS="-g -gnata -E -fprofile-arcs -ftest-coverage" \
	bin/xreq $(XREQ_FLAGS) -m -x feature_tests -o features/tests/cov features/*.feature
	$(CP) features/tests/cov/feature_tests $@

bin/unit_tests: bin/unit_tests.dbg
	@echo "LINK    $@"
	ln -s -f unit_tests.dbg $@

tests: bin/unit_tests bin/feature_tests

doc: dir README.html src/README.html reports/index.html
	
clean: cov-clean
	-gprclean -Pxreq.gpr    -Xtype=dynamic -Xmode=debug
	-gprclean -Pxreq.gpr    -Xtype=static  -Xmode=release
	-gprclean -Pxreq.gpr    -Xtype=dynamic -Xmode=coverage
	-gprclean -Pxreq.gpr    -Xtype=static  -Xmode=debug
	-gprclean -Pxreq.gpr    -Xtype=dynamic -Xmode=release
	-gprclean -Pxreq.gpr    -Xtype=static  -Xmode=coverage
	-gprclean -Plibxreq.gpr -Xtype=dynamic -Xmode=debug
	-gprclean -Plibxreq.gpr -Xtype=dynamic -Xmode=coverage
	-gprclean -Plibxreq.gpr -Xtype=dynamic -Xmode=release
	-gprclean -Pxreqlib.gpr -Xtype=dynamic -Xmode=debug
	-gprclean -Pxreqlib.gpr -Xtype=static  -Xmode=release
	-gprclean -Pxreqlib.gpr -Xtype=dynamic -Xmode=coverage
	-gprclean -Pxreqlib.gpr -Xtype=static  -Xmode=debug
	-gprclean -Pxreqlib.gpr -Xtype=dynamic -Xmode=release
	-gprclean -Pxreqlib.gpr -Xtype=static  -Xmode=coverage
	-$(RM) -rf tmp
	-$(RM) -rf obj/*/*
	-$(RM) -rf lib/*/*
	-$(RM) bin/*
	-$(RM) README.html
	-$(RM) src/README.html
	-$(RM) reports/*.aunit.xml
	-$(RM) reports/gnatcheck*.out
	-$(RM) reports/gnatcheck*.log
	-$(RM) reports/*.gcov
	-$(RM) reports/features*.html
	-$(RM) reports/features*.junit/*
	-$(RM) features/data/tmp-*
	-$(RM) features/data/step_definitions*/*.[od]
	-$(RM) features/data/step_definitions*/*.gcda
	-$(RM) features/data/step_definitions*/*.gcno
	-$(RM) -rf features/tests/*
	-$(RM) -rf tests/features/tests/*
	-find . -name "*~" -print0 | xargs -0 rm

_tests_requirements: bin lib

.PHONY: bin/xreq bin/unit_tests
.PHONY: all check-all dir lib bin gps-plugin tests bootstrap doc clean _tests_requirements


####################
##                ##
##    COVERAGE    ##
##                ##
####################

define _LCOV_ZERO
	@echo "LCOV    --zerocounters"
	-lcov -q -d obj/coverage --zerocounters
endef

LCOVFLAGS = -q -c -d obj/coverage
coverage/%.lcov.info:
	@echo "LCOV    $@"
	lcov $(LCOVFLAGS) -o $@

coverage/01-base.lcov.info:         LCOVFLAGS += -t Base --initial
coverage/10-unit.lcov.info:         LCOVFLAGS += -t Unit_Tests
coverage/20-cucumber-ada.lcov.info: LCOVFLAGS += -t Cucumber_Ada
coverage/21-cucumber-c.lcov.info:   LCOVFLAGS += -t Cucumber_C
coverage/22-xreq-ada.lcov.info:     LCOVFLAGS += -t XReq_Ada
coverage/23-xreq-c.lcov.info:       LCOVFLAGS += -t XReq_C

ifneq ($(cov_only_cucumber_c),)
cov_ignore_base=1
cov_ignore_ignore=1
cov_ignore_unit=1
cov_ignore_cucumber_ada=1
endif

ifneq ($(cov_only_cucumber_ada),)
cov_ignore_base=1
cov_ignore_ignore=1
cov_ignore_unit=1
cov_ignore_cucumber_c=1
endif

cov-init:
	-$(RM) -rf coverage/lcov.info coverage/*.lcov.info coverage/*

cov-test-base:
ifeq ($(cov_ignore_base),)
	$(_LCOV_ZERO)
	$(MAKE) coverage/01-base.lcov.info
endif

cov-test-ignore:
ifeq ($(cov_ignore_ignore),)
	@echo "COV     coverage/02-ignore.lcov.info"
	lcov -q -c -i -d obj/coverage -t "Ignored_Lines" -o coverage/02-ignore.lcov.info.tmp
	perl gcov-ignore.pl coverage/02-ignore.lcov.info.tmp > coverage/02-ignore.lcov.info
	-rm coverage/02-ignore.lcov.info.tmp
endif

cov-test-unit: bin/unit_tests.cov
ifeq ($(cov_ignore_unit),)
	$(_LCOV_ZERO)
	@echo "RUN     bin/unit_tests.cov"
	-bin/unit_tests.cov -text -o reports/test.aunit.txt > reports/test-debug.aunit.txt 2>&1
	$(MAKE) coverage/10-unit.lcov.info
endif

cov-cucumber-setup:
	@echo "RM      bin/xreq"
	-$(RM) bin/xreq
	@echo "LINK    bin/xreq"
	ln -s -f xreq.cov bin/xreq

cov-test-cucumber-ada: _tests_requirements bin/xreq.cov
ifeq ($(cov_ignore_cucumber_ada)$(cov_ignore_cucumber)$(cov_ignore_ada),)
	$(_LCOV_ZERO)
	@echo "MAKE    run-cucumber-ada"
	@-mode=coverage GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
	$(MAKE) run-cucumber-ada
	$(MAKE) coverage/20-cucumber-ada.lcov.info
endif

cov-test-cucumber-c: _tests_requirements bin/xreq.cov
ifeq ($(cov_ignore_cucumber_c)$(cov_ignore_cucumber)$(cov_ignore_c),)
	$(_LCOV_ZERO)
	@echo "MAKE    run-cucumber-c"
	@-mode=coverage LDFLAGS="-fprofile-arcs" CFLAGS="-ftest-coverage -fprofile-arcs -g" \
	$(MAKE) run-cucumber-c
	$(MAKE) coverage/21-cucumber-c.lcov.info
endif

cov-cucumber-teardown:
	@echo "RM      bin/xreq"
	-$(RM) bin/xreq
	@echo "MAKE    bin/xreq"
	$(MAKE) bin/xreq

cov-html:
	$(_LCOV_ZERO)
	@echo "LCOV    coverage/lcov.info"
	lcov -q $(foreach lcov,$(wildcard coverage/*.lcov.info),-a $(lcov)) -o coverage/lcov.info
	lcov --remove coverage/lcov.info '/opt/*' -o coverage/lcov.info
	lcov --remove coverage/lcov.info '/usr/*' -o coverage/lcov.info
	lcov --remove coverage/lcov.info '*~*'    -o coverage/lcov.info
	lcov --remove coverage/lcov.info '*__*'   -o coverage/lcov.info
	@echo "GENHTML coverage/lcov.info"
	genhtml --show-details --no-function-coverage -o coverage coverage/lcov.info || \
	genhtml --show-details -o coverage coverage/lcov.info

cov: bin/xreq.cov lib/coverage/libxreq.so bin/unit_tests.cov
ifneq ($(CONFIG),cov)
	$(MAKE) CONFIG=cov $@
else
	@echo "MAKE    cov-init"
	$(MAKE) cov-init
	@echo "MAKE    cov-test-base"
	$(MAKE) cov-test-base
	@echo "MAKE    cov-test-unit"
	$(MAKE) cov-test-unit
	@echo "MAKE    cov-test-ignore"
	$(MAKE) cov-test-ignore
	@echo "MAKE    cov-cucumber-setup"
	$(MAKE) cov-cucumber-setup
	@echo "MAKE    cov-test-cucumber-ada"
	$(MAKE) cov-test-cucumber-ada
	@echo "MAKE    cov-test-cucumber-c"
	$(MAKE) cov-test-cucumber-c
	@echo "MAKE    cov-cucumber-teardown"
	$(MAKE) cov-cucumber-teardown
	@echo "MAKE    cov-html"
	$(MAKE) cov-html
endif

cov-clean:
	-$(RM) coverage/*.lcov.info
	-$(RM) coverage/*.gcov

coverage: cov

.PHONY: cov cov-clean coverage cov-init cov-test-base cov-test-ignore cov-test-unit cov-test-cucumber-ada cov-html cov-cucumber-setup cov-cucumber-teardown

#################
##             ##
##    TESTS    ##
##             ##
#################


features/tests/suite.gpr: bin/xreq $(wildcard features/*.feature)
	@echo "XREQ    $@"
	bin/xreq $(XREQ_FLAGS) -x suite $(wildcard features/*.feature)

features/tests/suite: features/tests/suite.gpr
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pfeatures/tests/suite.gpr
.PHONY: features/tests/suite

_cucumber_clean_rerun:
	-$(RM) -f cucumber-rerun.txt
	-$(RM) -f cucumber-rerun-lang.txt

run-cucumber:
	$(MAKE) run-cucumber-ada
	$(MAKE) run-cucumber-c

run-cucumber-wip: _tests_requirements _cucumber_clean_rerun
	@echo
	@echo "#########################################"
	@echo "##  Run cucumber for work in progress  ##"
	@echo "#########################################"
	cucumber -w -t "@wip" -f progress features/*.feature

run-cucumber-c: _tests_requirements _cucumber_clean_rerun
	@echo
	@echo "##########################"
	@echo "##  Run cucumber for C  ##"
	@echo "##########################"
	@echo
	echo C > cucumber-rerun-lang.txt
	XREQ_LANG=C \
	cucumber -t "~@wip" -t "~@bootstrap" -t "@lang-C,~@lang" -f progress -f rerun -o cucumber-rerun.txt features/*.feature

run-cucumber-ada: _tests_requirements _cucumber_clean_rerun
	@echo
	@echo "############################"
	@echo "##  Run cucumber for Ada  ##"
	@echo "############################"
	@echo
	echo Ada > cucumber-rerun-lang.txt
	XREQ_LANG=Ada \
	cucumber -t "~@wip" -t "~@bootstrap" -t "@lang-Ada,~@lang" -f progress -f rerun -o cucumber-rerun.txt features/*.feature

run-bootstrap: _tests_requirements
	cucumber -t "~@wip" -t "@bootstrap" features/*.feature

rerun-cucumber: _tests_requirements
	@echo
	@echo "##########################################"
	@echo "##  Run cucumber scenarios that failed  ##"
	@echo "##########################################"
	@echo
	touch cucumber-rerun.txt
	@if [ -s cucumber-rerun.txt ]; then \
		xargs echo XREQ_LANG=$$(cat cucumber-rerun-lang.txt) cucumber < cucumber-rerun.txt; \
		XREQ_LANG=$$(cat cucumber-rerun-lang.txt) xargs cucumber < cucumber-rerun.txt; \
	fi
	-$(RM) -f cucumber-rerun.txt

run-xreq:
	$(MAKE) run-xreq-ada
	$(MAKE) run-xreq-c

run-xreq-wip: bin/feature_tests _tests_requirements
	@echo
	@echo "#####################################"
	@echo "##  Run XReq for work in progress  ##"
	@echo "#####################################"
	@echo
	$< -t '@wip'

run-xreq-ada: bin/feature_tests _tests_requirements
	@echo
	@echo "########################"
	@echo "##  Run XReq for Ada  ##"
	@echo "########################"
	@echo
	XREQ_LANG=Ada $< -t '~@wip+~@bootstrap+~@lang,@lang-Ada'

run-xreq-c: bin/feature_tests _tests_requirements
	@echo
	@echo "######################"
	@echo "##  Run XReq for C  ##"
	@echo "######################"
	@echo
	XREQ_LANG=C $< -t '~@wip+~@bootstrap+~@lang,@lang-C'

run-unit: tests bin
	@echo
	@echo "######################"
	@echo "##  Run unit tests  ##"
	@echo "######################"
	@echo
	bin/unit_tests

.PHONY: run-cucumber rerun-cucumber run-cucumber-wip run-cucumber-c run-cucumber-ada _cucumber_clean_rerun run-xreq run-xreq-c run-xreq-ada run-unit

########################
##                    ##
##    TEST REPORTS    ##
##                    ##
########################

reports/cucumber-ada.html: _tests_requirements
	@echo "CUKE    $@"
	if XREQ_LANG=Ada cucumber -t "~@wip" -t "~@bootstrap" -t "@lang-Ada,~@lang" -f html -o $@ features/*.feature; then \
		echo OK >$(@:%.html=%.status); \
	else \
		: >$(@:%.html=%.status); \
	fi

reports/cucumber-c.html: _tests_requirements
	@echo "CUKE    $@"
	if XREQ_LANG=C cucumber -t "~@wip" -t "~@bootstrap" -t "@lang-C,~@lang" -f html -o $@ features/*.feature; then \
		echo OK >$(@:%.html=%.status); \
	else \
		: >$(@:%.html=%.status); \
	fi

reports/xreq-ada.html: bin/feature_tests _tests_requirements
	@echo "FEATURE $@"
	if XREQ_LANG=Ada $< -d -t "~@wip+~@bootstrap+~@lang,@lang-Ada" -f html -o $@; then \
		echo OK >$(@:%.html=%.status); \
	else \
		: >$(@:%.html=%.status); \
	fi

reports/xreq-c.html: bin/feature_tests _tests_requirements
	@echo "FEATURE $@"
	if XREQ_LANG=C $< -d -t "~@wip+~@bootstrap+~@lang,@lang-C" -f html -o $@; then \
		echo OK >$(@:%.html=%.status); \
	else \
		: >$(@:%.html=%.status); \
	fi

reports/%.status:
	$(MAKE) $(@:%.status=%.html)

reports/unit.status:
	$(MAKE) reports/unit.txt

reports/unit.txt reports/unit-debug.txt: bin/unit_tests
	@echo "AUNIT   $@"
	if bin/unit_tests -text -o reports/unit.txt >reports/unit-debug.txt 2>&1; then \
		echo OK >reports/unit.status; \
	else \
		: >reports/unit.status; \
	fi

report-cucumber: reports/cucumber-ada.html reports/cucumber-c.html
report-xreq: reports/xreq-ada.html reports/xreq-c.html

report-c:   reports/cucumber-c.html reports/xreq-c.html  
report-ada: reports/cucumber-ada.html reports/xreq-ada.html

report-unit: reports/unit.txt

report-all: report-cucumber report-xreq report-unit

.PHONY: reports/xreq-ada.html reports/xreq-c.html reports/cucumber-ada.html reports/cucumber-c.html report-unit report-all report-xreq report-cucumber report-c report-ada



#######################
##                   ##
##    CHECK TESTS    ##
##                   ##
#######################

define _CHECK_TESTS
	@echo "CHECK   $+"; \
	exit_code=0; \
	for t in $+; do \
		if ! [ -f "$$t" ]; then \
			echo "        MISS $$t"; \
			let exit_code=exit_code+2; \
		elif [ -s "$$t" ]; then \
			echo "        PASS $$t"; \
		else \
			echo "        FAIL $$t"; \
			let exit_code=exit_code+1; \
		fi; \
	done; \
	exit $$exit_code
endef

check-cucumber-ada: reports/cucumber-ada.status ; $(_CHECK_TESTS)
check-cucumber-c:   reports/cucumber-c.status   ; $(_CHECK_TESTS)
check-xreq-ada:     reports/xreq-ada.status     ; $(_CHECK_TESTS)
check-xreq-c:       reports/xreq-c.status       ; $(_CHECK_TESTS)

check-tests: reports/cucumber-ada.status reports/cucumber-c.status reports/xreq-ada.status reports/xreq-c.status reports/unit.status
	$(_CHECK_TESTS)

.PHONY: check-tests check-cucumber-ada check-cucumber-c check-xreq-ada check-xreq-c

########################
##                    ##
##    STYLE CHECKS    ##
##                    ##
########################


gnatcheck: dir
	@echo "GNAT    CHECK   xreq.gpr"
	cd reports && gnat check -P../xreq.gpr -U -rules -from=../gnatcheck.rules

########################
##                    ##
##    TEST REPORTS    ##
##                    ##
########################

test-report:
	-$(MAKE) coverage
	-$(MAKE) test-report-unit
	-$(MAKE) test-report-cucumber
	-#$(MAKE) test-report-xreq # Not necessary, bootstrap.feature does it

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

test-report-xreq: bin reports/features-xreq.html


reports/features-xreq.html: bin/feature_tests
	-bin/feature_tests -f html -o reports/features-xreq.html

check: gnatcheck coverage run-cucumber run-unit

.PHONY: gnatcheck test-report test-report-cucumber test-report-unit check

###################
##               ##
##    INSTALL    ##
##               ##
###################

install: bin/xreq.$(INSTALL_CONFIG) lib/$(INSTALL_MODE)/libxreq.so install-gps
	$(INSTALL) -D bin/xreq.$(INSTALL_CONFIG) $(DESTDIR)$(BINDIR)/xreq
	$(INSTALL) -m644 -D data/xreqlib.gpr $(DESTDIR)$(GPRDIR)/xreqlib.gpr
	$(INSTALL) -d $(DESTDIR)$(INCLUDEDIR)/xreqlib
	$(CP) src/common/*.ad[bs] $(DESTDIR)$(INCLUDEDIR)/xreqlib
	$(CP) src/lib/*.ad[bs] $(DESTDIR)$(INCLUDEDIR)/xreqlib
	$(CP) src/lib/static/*.ad[bs] $(DESTDIR)$(INCLUDEDIR)/xreqlib
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/xreqlib
	$(CP) lib/$(INSTALL_MODE)/* $(DESTDIR)$(LIBDIR)/xreqlib
	$(INSTALL) -m755 -D lib/$(INSTALL_MODE)/libxreq.so $(DESTDIR)$(LIBDIR)/libxreq.so
	$(INSTALL) -m644 -D src/lib/xreq.h $(DESTDIR)$(INCLUDEDIR)/xreq.h
	@echo '------------------------------------------------------------------'
	@echo '--  XReq has now been installed.'
	@echo '------------------------------------------------------------------'
	@echo '--  To be able to use the xreq binary, you may need to update'
	@echo '--  your PATH to point to'
	@echo '--  $(DESTDIR)$(BINDIR)'
	@echo '------------------------------------------------------------------'
	@echo '--  To be able to use the library for Ada, you may need to update'
	@echo '--  your ADA_PROJECT_PATH or GPR_PROJECT_PATH to point to the path'
	@echo '--  $(DESTDIR)$(GPRDIR)'
	@echo '------------------------------------------------------------------'
	@echo '--  To be able to use the library for C, you may need to update'
	@echo '--  your LD_LIBRARY_PATH to point to the path'
	@echo '--  $(DESTDIR)$(LIBDIR)'
	@echo '------------------------------------------------------------------'

install-gps: lib/gps/libxreqgps.so
	$(INSTALL) -m644 data/gpr-plug-in/xreq.xml      $(DESTDIR)$(GPSDATADIR)/plug-ins/xreq.xml
	$(INSTALL) -m644 data/gpr-plug-in/xreq.py       $(DESTDIR)$(GPSDATADIR)/plug-ins/xreq.py
	$(INSTALL) -m644 data/gpr-plug-in/feature-lang.xml $(DESTDIR)$(GPSDATADIR)/plug-ins/feature-lang.xml
	$(INSTALL) -m755 lib/gps/libxreqgps.so          $(DESTDIR)$(LIBDIR)/libxreqgps.so

uninstall: uninstall-gps
	-$(RM) -rf $(DESTDIR)$(BINDIR)/xreq
	-$(RM) -rf $(DESTDIR)$(GPRDIR)/xreqlib.gpr
	-$(RM) -rf $(DESTDIR)$(INCLUDEDIR)/xreqlib
	-$(RM) -rf $(DESTDIR)$(LIBDIR)/xreqlib
	-$(RM) -rf $(DESTDIR)$(LIBDIR)/libxreq.so
	-$(RM) -rf $(DESTDIR)$(INCLUDEDIR)/xreq.h
	-$(RM) -rf $(DESTDIR)$(DOCDIR)
	-$(RM) -rf $(DESTDIR)$(DATADIR)/XReq

uninstall-gps:
	-$(RM) -rf $(DESTDIR)$(LIBDIR)/libxreqgps.so
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/plug-ins/xreq.xml
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/plug-ins/xreq.py
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/plug-ins/feature-lang.xml

install-gps-local:
	ln -sf "`pwd`"/data/gpr-plug-in/*.{xml,py} ~/.gps/plug-ins
	ln -sf "`pwd`"/lib/gps/libxreqgps.so ~/.local/lib

uninstall-gps-local:
	-$(RM) ~/.gps/plug-ins/xreq.xml
	-$(RM) ~/.gps/plug-ins/xreq.py
	-$(RM) ~/.gps/plug-ins/feature-lang.xml
	-$(RM) ~/.local/lib/libxreqgps.so

.PHONY: install install-gps uninstall install-gps-local uninstall-gps uninstall-gps-local

################
##            ##
##    HELP    ##
##            ##
################

show-ignored-coverage:
	find src -name "*.ad[bs]" -print0 | xargs -0 grep -Rn GCOV_IGNORE

help:
	@echo "make TARGET"
	@echo
	@echo "Targets:"
	@echo
	@echo "Building:"
	@echo "    all:            Build everything     [bin gps-plugin tests doc]"
	@echo "    bin:            Build XReq           [bin/xreq]"
	@echo "    bin:            Build XReq libraries [lib]"
	@echo "    gps-plugin:     Build GPS plugin     [lib/gps]"
	@echo "    tests:          Build tests          [bin/*_tests]"
	@echo "    doc:            Build documentation  [README.html]"
	@echo "Checking:"
	@echo "    coverage:       Run coverage tests   [coverage/]"
	@echo "    gnatcheck:      Run gnatcheck        [reports/gnatcheck*]"
	@echo "    report-all:     Create HTML reports  [reports/*.html]"
	@echo "    check-tests:    Check tests passed"
	@echo "Testing (interactive use):"
	@echo "    run-unit:       Run all unit tests"
	@echo "    run-xreq:       Run all cucumber tests with XReq"
	@echo "    run-cucumber:   Run all cucumber tests"
	@echo "    rerun-cucumber: Run last failed cucumber tests"
	@echo "Other:"
	@echo "    clean:          Clean project"
	@echo "    install:        Install XReq"
	@echo "    unnstall:       Uninstall XReq"
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
	@echo "    INCLUDEDIR      path for C header files   [$(INCLUDEDIR)]"
	@echo "    LIBDIR          path for library          [$(LIBDIR)]"
	@echo "    GPRDIR          GNAT Project files        [$(GPRDIR)]"
	@echo "    DOCDIR          Documentation directory   [$(DOCDIR)]"
	@echo "    DATADIR         Read only architecture-independant data"
	@echo "                                              [$(DATADIR)]"
	@echo "    GPSDATADIR      GPS plugin data           [$(GPSDATADIR)]"

.PHONY: help show-ignored-coverage

src/lib/xreqlib-format_html_template.ads src/lib/xreqlib-format_html_template.adb: src/xreq-report.template.html ./template.pl
	./template.pl $< XReqLib.Format_HTML_Template $@

####################
##                ##
##    MARKDOWN    ##
##                ##
####################

MARKDOWN_URL=http://daringfireball.net/projects/downloads/Markdown_1.0.1.zip
MARKDOWN_DIR=Markdown_1.0.1
MARKDOWN_CMDLINE=./Markdown.pl <$< >$@

Markdown.zip:
	@echo "WGET    $@"
	wget $(MARKDOWN_URL) -O $@

Markdown.pl:
	@echo "UNZIP   $@"
	$(MAKE) Markdown.zip
	unzip -u -j Markdown.zip $(MARKDOWN_DIR)/$@
	@echo "CHMOD   $@"
	chmod +x $@
	-$(RM) Markdown.zip

%.html: %.mdwn Markdown.pl
	@echo "MDWN    $@"
	$(MARKDOWN_CMDLINE)

%.html: % Markdown.pl
	@echo "MDWN    $@"
	$(MARKDOWN_CMDLINE)
