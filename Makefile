##                         Copyright (C) 2010, Sogilis                       ##

ifeq ($(shell which redo 2>/dev/null),)
REDO=redoconf/minimal-redo
else
REDO=redo
endif

GNATMAKE=gnatmake
TEST_SUITES=test coverage
CONFIG=dbg
INSTALL_CONFIG=rel
MODE=
LIBEXT=

UNIXNAME=xreq
TIMESTAMP=$(shell date +%Y%m%d%H%M%S)
VERSION=$(TIMESTAMP)
ifeq ($(VERSION),)
ARCHIVENAME=$(UNIXNAME)
else
ARCHIVENAME=$(UNIXNAME)-$(VERSION)
endif

ifeq ($(shell uname -s),Darwin)
SUF_SO=dylib
else
SUF_SO=so
endif

DOC_FILES=README.html HISTORY.html src/README.html reports/index.html

# dynamic makes gcov unhappy: hidden symbol `__gcov_merge_add' is referenced by
# DSO (Dynamic Shared Object).
LIBTYPE=static

# release doesn't work, undefined reference to 
# `gnat__traceback__symbolic__symbolic_traceback__2'
INSTALL_CONFIG=dbg

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
ifeq ($(VERBOSE),0)
GPRBUILD_FLAGS=-p
XREQ_FLAGS=
else
GPRBUILD_FLAGS=-p -v
XREQ_FLAGS=
endif
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
DOCDIR     = $(DATADIR)/doc/XReq
ifneq ($(PREFIX_GPS),)
GPSDATADIR = $(PREFIX_GPS)/share/gps
else
GPSDATADIR =
endif

all: bin lib gps-plugin tests doc
	@echo
	@echo "########################################################"
	@echo "##                                                    ##"
	@echo "##    Run 'make help' to get help on this Makefile    ##"
	@echo "##                                                    ##"
	@echo "########################################################"
	@echo
	@echo "You built:"
	@echo "  - Executable:     bin/xreq"
	@echo "  - Library:        lib/$(MODE)/libxreqlib.$(LIBEXT)"
	@echo "                    lib/$(MODE)/libxreq.$(SUF_SO)"
	@echo "  - GPS Plug-In:    lib/gps/libxreqgps.$(SUF_SO)"
	@echo "  - Tests:          bin/unit_tests"
	@echo "                    bin/feature_tests"
	@echo "  - Documentation:  $(DOC_FILES)"
	@echo

build: bin/xreq.$(INSTALL_CONFIG) lib/$(INSTALL_MODE)/libxreqlib.$(LIBEXT) lib/$(INSTALL_MODE)/libxreq.$(SUF_SO) lib/gps/libxreqgps.$(SUF_SO)
	@echo
	@echo "########################################################"
	@echo "##                                                    ##"
	@echo "##    Run 'make help' to get help on this Makefile    ##"
	@echo "##      You built XReq binaries ready to install      ##"
	@echo "##                                                    ##"
	@echo "########################################################"
	@echo
	@echo "You built:"
	@echo "  - Executable:     bin/xreq.$(INSTALL_CONFIG)"
	@echo "  - Library:        lib/$(INSTALL_MODE)/libxreqlib.$(LIBEXT)"
	@echo "                    lib/$(INSTALL_MODE)/libxreq.$(SUF_SO)"
	@echo "  - GPS Plug-In:    lib/gps/libxreqgps.$(SUF_SO)"
	@echo
	@echo "You may install XReq running as a priviledged user:"
	@echo "    make PREFIX=$(PREFIX) install"
	@echo



check-all: all build gnatcheck run-unit run-features coverage

$(VERBOSE).SILENT:

########################
##                    ##
##    MAIN TARGETS    ##
##                    ##
########################

dir:
	@-mkdir -p lib/gps
	@-mkdir -p lib/release
	@-mkdir -p lib/debug
	@-mkdir -p lib/coverage
	@-mkdir -p bin
	@-mkdir -p doc
	@-mkdir -p reports
	@-mkdir -p coverage
	@-mkdir -p obj/release
	@-mkdir -p obj/dynamic-release
	@-mkdir -p obj/debug
	@-mkdir -p obj/dynamic-debug
	@-mkdir -p obj/coverage
	@-mkdir -p obj/dynamic-coverage

bin: bin/xreq

lib: lib/$(MODE)/libxreqlib.$(LIBEXT) lib/$(MODE)/libxreq.$(SUF_SO)

lib/debug/libxreq.$(SUF_SO): dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Plibxreq.gpr -Xtype=dynamic -Xmode=debug
	$(CP) lib/external-dynamic-debug/$(notdir $@) $@

lib/release/libxreq.$(SUF_SO): dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Plibxreq.gpr -Xtype=dynamic -Xmode=release
	$(CP) lib/external-dynamic-release/$(notdir $@) $@

lib/coverage/libxreq.$(SUF_SO): dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Plibxreq.gpr -Xtype=dynamic -Xmode=coverage
	$(CP) lib/external-dynamic-coverage/$(notdir $@) $@

lib/debug/libxreqlib.$(SUF_SO): dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=dynamic -Xmode=debug
	$(CP) lib/dynamic-debug/$(notdir $@) $@
	$(CP) lib/dynamic-debug/*.ali $(dir $@)
	chmod +r $(dir $@)/*.ali

lib/release/libxreqlib.$(SUF_SO): dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=dynamic -Xmode=release
	$(CP) lib/dynamic-release/$(notdir $@) $@
	$(CP) lib/dynamic-release/*.ali $(dir $@)
	chmod +r $(dir $@)/*.ali

lib/coverage/libxreqlib.$(SUF_SO): dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=dynamic -Xmode=coverage
	$(CP) lib/dynamic-coverage/$(notdir $@) $@
	$(CP) lib/dynamic-coverage/*.ali $(dir $@)
	chmod +r $(dir $@)/*.ali

lib/debug/libxreqlib.a: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=static  -Xmode=debug
	$(CP) lib/static-debug/$(notdir $@) $@
	$(CP) lib/static-debug/*.ali $(dir $@)
	chmod +r $(dir $@)/*.ali

lib/release/libxreqlib.a: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=static  -Xmode=release
	$(CP) lib/static-release/$(notdir $@) $@
	$(CP) lib/static-release/*.ali $(dir $@)
	chmod +r $(dir $@)/*.ali

lib/coverage/libxreqlib.a: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreqlib.gpr -Xtype=static  -Xmode=coverage
	$(CP) lib/static-coverage/$(notdir $@) $@
	$(CP) lib/static-coverage/*.ali $(dir $@)
	chmod +r $(dir $@)/*.ali

bin/xreq.cov: lib/coverage/libxreqlib.$(LIBEXT)
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreq.gpr    -Xtype=$(LIBTYPE) -Xmode=coverage
	test -f $@

bin/xreq.rel: lib/release/libxreqlib.$(LIBEXT)
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreq.gpr    -Xtype=$(LIBTYPE) -Xmode=release
	test -f $@

bin/xreq.dbg: lib/debug/libxreqlib.$(LIBEXT)
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pxreq.gpr    -Xtype=$(LIBTYPE) -Xmode=debug
	test -f $@


bin/xreq: bin/xreq.$(CONFIG)
	@echo "LINK    $@"
	-rm -f bin/xreq
	ln -s xreq.$(CONFIG) bin/xreq

lib/gps/libxreqgps.$(SUF_SO): dir
	@echo "LINK    $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Pgps_plugin.gpr -Xmode=release
	test -f $@
	#$(MAKE) -C src/gps libgprcustom.$(SUF_SO) && mv src/gps/libgprcustom.$(SUF_SO) $@

gps-plugin: lib/gps/libxreqgps.$(SUF_SO)

bin/unit_tests: bin/unit_tests.dbg
	@echo "LINK    $@"
	ln -s -f unit_tests.dbg $@

bin/unit_tests.dbg: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Punit_tests.gpr -Xmode=debug
	test -f $@

bin/unit_tests.cov: dir
	@echo "GNAT    BUILD   $@"
	$(GPRBUILD) $(GPRBUILD_FLAGS) -Punit_tests.gpr -Xmode=coverage
	test -f $@

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

### gprbuild targets are phony ###
.PHONY: lib/debug/libxreq.$(SUF_SO) lib/release/libxreq.$(SUF_SO) lib/coverage/libxreq.$(SUF_SO)
.PHONY: lib/debug/libxreqlib.$(SUF_SO) lib/release/libxreqlib.$(SUF_SO) lib/coverage/libxreqlib.$(SUF_SO)
.PHONY: lib/debug/libxreqlib.a lib/release/libxreqlib.a lib/coverage/libxreqlib.a
.PHONY: bin/xreq.cov bin/xreq.rel bin/xreq.dbg
.PHONY: bin/unit_tests.dbg bin/unit_tests.cov
.PHONY: bin/feature_tests.dbg bin/feature_tests.cov

tests: bin/feature_tests # bin/unit_tests
	@echo "WARNING: Unit tests were not compiled because AUnit version is out of date"

doc:
	$(REDO) doc
	
clean:
	$(REDO) clean

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


ifneq ($(CONFIG),cov)
cov cov-html cov-init cov-test-cucumber-ada cov-test-cucumber-c cov-test-unit cov-test-ignore cov-test-base cov-zero:
	$(MAKE) CONFIG=cov $@
else

cov-zero:
	$(_LCOV_ZERO)

cov-requirement: bin/xreq.cov lib/coverage/libxreq.$(SUF_SO) bin/unit_tests.cov

cov-cucumber-setup:
	@echo "RM      bin/xreq"
	-$(RM) bin/xreq
	@echo "LINK    bin/xreq"
	ln -s -f xreq.cov bin/xreq

cov-init:
	@echo
	@echo "#######################################"
	@echo "##  Initialize coverage environment  ##"
	@echo "#######################################"
	@echo
	$(MAKE) cov-requirement
	$(MAKE) cov-cucumber-setup
	-$(RM) -rf coverage/lcov.info coverage/*.lcov.info coverage/*

cov-test-base:
ifeq ($(cov_ignore_base),)
	@echo
	@echo "###################################"
	@echo "##  Generate base coverage data  ##"
	@echo "###################################"
	@echo
	$(_LCOV_ZERO)
	$(MAKE) coverage/01-base.lcov.info
endif

cov-test-ignore:
ifeq ($(cov_ignore_ignore),)
	@echo
	@echo "##########################################"
	@echo "##  Create ignored lines coverage data  ##"
	@echo "##########################################"
	@echo
	@echo "COV     coverage/02-ignore.lcov.info"
	lcov -q -c -i -d obj/coverage -t "Ignored_Lines" -o coverage/02-ignore.lcov.info.tmp
	perl tools/gcov-ignore.pl coverage/02-ignore.lcov.info.tmp > coverage/02-ignore.lcov.info
	-rm coverage/02-ignore.lcov.info.tmp
endif

cov-test-unit: bin/unit_tests.cov
ifeq ($(cov_ignore_unit),)
	@echo
	@echo "######################"
	@echo "##  Run unit tests  ##"
	@echo "######################"
	@echo
	$(_LCOV_ZERO)
	@echo "RUN     bin/unit_tests.cov"
	-bin/unit_tests.cov -text -o reports/test.aunit.txt > reports/test-debug.aunit.txt 2>&1
	$(MAKE) coverage/10-unit.lcov.info
endif

cov-test-cucumber-ada: _tests_requirements bin/xreq.cov
ifeq ($(cov_ignore_cucumber_ada)$(cov_ignore_cucumber)$(cov_ignore_ada),)
	@echo
	@echo "############################"
	@echo "##  Run Cucumber for Ada  ##"
	@echo "############################"
	@echo
	$(_LCOV_ZERO)
	@echo "MAKE    run-cucumber-ada"
	@-mode=coverage GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
	$(MAKE) run-cucumber-ada
	$(MAKE) coverage/20-cucumber-ada.lcov.info
endif

cov-test-cucumber-c: _tests_requirements bin/xreq.cov
ifeq ($(cov_ignore_cucumber_c)$(cov_ignore_cucumber)$(cov_ignore_c),)
	@echo
	@echo "##########################"
	@echo "##  Run Cucumber for C  ##"
	@echo "##########################"
	@echo
	$(_LCOV_ZERO)
	@echo "MAKE    run-cucumber-c"
	@-mode=coverage LDFLAGS="-fprofile-arcs" CFLAGS="-ftest-coverage -fprofile-arcs -g" \
	$(MAKE) run-cucumber-c
	$(MAKE) coverage/21-cucumber-c.lcov.info
endif

cov-cucumber-teardown:
	@echo "RM      bin/xreq"
	-$(RM) bin/xreq
	#@echo "MAKE    bin/xreq"
	#$(MAKE) bin/xreq

cov-html:
	@echo
	@echo "#####################################"
	@echo "##  Generate HTML coverage report  ##"
	@echo "#####################################"
	@echo
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

cov: cov-requirement
	@echo "MAKE    cov-init"
	$(MAKE) cov-init
	@echo "MAKE    cov-test-unit"
	$(MAKE) cov-test-unit
	@echo "MAKE    cov-test-ignore"
	$(MAKE) cov-test-ignore
	@echo "MAKE    cov-test-cucumber-ada"
	$(MAKE) cov-test-cucumber-ada
	@echo "MAKE    cov-test-cucumber-c"
	$(MAKE) cov-test-cucumber-c
	@echo "MAKE    cov-html"
	$(MAKE) cov-html
	@echo "MAKE    cov-cucumber-teardown"
	$(MAKE) cov-cucumber-teardown

endif # ifneq ($(CONFIG),cov)

cov-clean:
	-$(RM) coverage/*.lcov.info
	-$(RM) coverage/*.gcov

cov-help:
	@echo "make CONFIG=cov TARGET"
	@echo
	@echo "    It's best to clean the project before executing coverage."
	@echo
	@echo "TARGETS:"
	@echo
	@echo "Housekeeping:"
	@echo "    cov-init        Initialize coverage environment"
	@echo "    cov-html        Generate HTML report"
	@echo "    cov-clean       Clean coverage files"
	@echo "Tests:"
	@echo "    cov-test-base   0% coverage test"
	@echo "    cov-test-unit   Coverage for unit tests"
	@echo "    cov-test-ignore Generate fake test results for ignored lines"
	@echo "    cov-test-cucumber-ada   Coverage for Cucumber Ada tests"
	@echo "    cov-test-cucumber-c     Coverage for Cucumber C tests"
	@echo "Low level:"
	@echo "    cov-zero                Zero counters for coverage"
	@echo "    coverage/%.lcov.info    Generate lcov report from gcov counters"
	@echo
	@echo "VARIABLES:"
	@echo
	@echo "    cov_only_cucumber_c"
	@echo "    cov_only_cucumber_ada"
	@echo "    cov_ignore_base"
	@echo "    cov_ignore_unit"
	@echo "    cov_ignore_ignore"
	@echo "    cov_ignore_cucumber"
	@echo "    cov_ignore_cucumber_ada"
	@echo "    cov_ignore_cucumber_c"
	@echo "    cov_ignore_ada"
	@echo "    cov_ignore_c"
	@echo

coverage: cov

.PHONY: cov-zero cov-help cov cov-requirement cov-clean coverage cov-init cov-test-base cov-test-ignore cov-test-unit cov-test-cucumber-ada cov-html cov-cucumber-setup cov-cucumber-teardown

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
	-$(RM) -f .cucumber-rerun.txt
	-$(RM) -f .cucumber-rerun-lang.txt

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
	echo C > .cucumber-rerun-lang.txt
	XREQ_LANG=C \
	cucumber -t "~@wip" -t "~@bootstrap" -t "@lang-C,~@lang" -f progress -f rerun -o .cucumber-rerun.txt features/*.feature

run-cucumber-ada: _tests_requirements _cucumber_clean_rerun
	@echo
	@echo "############################"
	@echo "##  Run cucumber for Ada  ##"
	@echo "############################"
	@echo
	echo Ada > .cucumber-rerun-lang.txt
	XREQ_LANG=Ada \
	cucumber -t "~@wip" -t "~@bootstrap" -t "@lang-Ada,~@lang" -f progress -f rerun -o .cucumber-rerun.txt features/*.feature

run-bootstrap: _tests_requirements
	cucumber -t "~@wip" -t "@bootstrap" features/*.feature

rerun-cucumber: _tests_requirements
	@echo
	@echo "##########################################"
	@echo "##  Run cucumber scenarios that failed  ##"
	@echo "##########################################"
	@echo
	touch .cucumber-rerun.txt
	@if [ -s .cucumber-rerun.txt ]; then \
		xargs echo XREQ_LANG=$$(cat .cucumber-rerun-lang.txt) cucumber < .cucumber-rerun.txt; \
		XREQ_LANG=$$(cat .cucumber-rerun-lang.txt) xargs cucumber < .cucumber-rerun.txt; \
	fi
	-$(RM) -f .cucumber-rerun.txt

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


run-features:
	$(MAKE) run-cucumber
	$(MAKE) run-xreq

run-features-c:
	$(MAKE) run-cucumber-c
	$(MAKE) run-xreq-c

run-features-ada:
	$(MAKE) run-cucumber-ada
	$(MAKE) run-xreq-ada

run-unit: bin/unit_tests
	@echo
	@echo "######################"
	@echo "##  Run unit tests  ##"
	@echo "######################"
	@echo
	bin/unit_tests

.PHONY: run-cucumber rerun-cucumber run-cucumber-wip run-cucumber-c run-cucumber-ada _cucumber_clean_rerun run-xreq run-xreq-c run-xreq-ada run-unit run-features run-features-c run-features-ada

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

report-cucumber-ada: reports/cucumber-ada.html
report-cucumber-c: reports/cucumber-c.html
report-xreq-ada: reports/xreq-ada.html
report-xreq-c: reports/xreq-c.html

report-cucumber: report-cucumber-ada report-cucumber-c
report-xreq: report-xreq-ada report-xreq-c

report-c:   report-cucumber-c report-xreq-c
report-ada: report-cucumber-ada report-xreq-ada


report-unit: reports/unit.txt

report-all: report-cucumber report-xreq report-unit

.PHONY: reports/xreq-ada.html reports/xreq-c.html reports/cucumber-ada.html reports/cucumber-c.html report-unit report-all report-xreq report-cucumber report-c report-ada report-cucumber-ada report-cucumber-c report-xreq-ada report-xreq-c



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
	cd reports && gnat check -P../xreq.gpr -U -rules -from=../data/gnatcheck.rules

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

install: install-bin install-lib install-gps install-gpr
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
	@echo '--  To be able to use the library, you may need to update your'
	@echo '--  LD_LIBRARY_PATH to point to the path'
	@echo '--  $(DESTDIR)$(LIBDIR)'
	@echo '------------------------------------------------------------------'

install-lib: #lib/$(INSTALL_MODE)/libxreq.$(SUF_SO) lib/$(INSTALL_MODE)/libxreqlib.$(LIBEXT)
	#
	# Installing GPR project file in $(GPRDIR)
	#
	# mkdir -p $(DESTDIR)$(GPRDIR)
	# $(INSTALL) -m644 data/xreqlib.gpr $(DESTDIR)$(GPRDIR)/xreqlib.gpr
	$(INSTALL) -d $(DESTDIR)$(GPRDIR)
	sed -e 's|%ADAINCLUDEDIR%|$(INCLUDEDIR)/xreqlib|g' \
	    -e 's|%ADALIBDIR%|$(LIBDIR)/xreqlib|g' \
	    -e 's|%ADALIBKIND%|$(LIBTYPE)|g' \
	    data/xreqlib-template.gpr > $(DESTDIR)$(GPRDIR)/xreqlib.gpr
	#
	# Installing source files in $(INCLUDEDIR)/xreqlib
	#
	$(RM) -rf $(DESTDIR)$(INCLUDEDIR)/xreqlib
	$(INSTALL) -d $(DESTDIR)$(INCLUDEDIR)/xreqlib
	$(CP) src/common/*.ad[bs]     $(DESTDIR)$(INCLUDEDIR)/xreqlib
	$(CP) src/lib/*.ad[bs]        $(DESTDIR)$(INCLUDEDIR)/xreqlib
	$(CP) src/lib/static/*.ad[bs] $(DESTDIR)$(INCLUDEDIR)/xreqlib
	#
	# Installing Ada library (libxreqlib) in $(LIBDIR) and $(LIBDIR)/xreqlib
	#
	$(RM) -rf $(DESTDIR)$(LIBDIR)/xreqlib
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/xreqlib
	$(CP) lib/$(INSTALL_MODE)/libxreqlib.* lib/$(INSTALL_MODE)/*.ali $(DESTDIR)$(LIBDIR)/xreqlib
ifeq ($(LIBTYPE),dynamic)
	# Copy the dynamic library to the libdir as well to get the executable
	# find it. Don't know how to tell the linker in the project file to look
	# in $(LIBDIR) instead of $(LIBDIR)/xreqlib. And don't want to install
	# ALI files directly in $(LIBDIR)
	$(CP) lib/$(INSTALL_MODE)/libxreqlib.* $(DESTDIR)$(LIBDIR)
endif
	#
	# Installing C library (libxreq) in $(LIBDIR) and C Header files in $(INCLUDEDIR)
	#
	mkdir -p $(DESTDIR)$(LIBDIR) $(DESTDIR)$(INCLUDEDIR)
	$(INSTALL) -m755 lib/$(INSTALL_MODE)/libxreq.$(SUF_SO) $(DESTDIR)$(LIBDIR)/libxreq.$(SUF_SO)
	$(INSTALL) -m644 src/lib/xreq.h $(DESTDIR)$(INCLUDEDIR)/xreq.h

install-bin: #bin/xreq.$(INSTALL_CONFIG)
	mkdir -p $(DESTDIR)$(BINDIR)
	$(INSTALL) bin/xreq.$(INSTALL_CONFIG) $(DESTDIR)$(BINDIR)/xreq


install-gps: #lib/gps/libxreqgps.$(SUF_SO)
ifneq ($(GPSDATADIR),)
	$(INSTALL) -m644 data/gps-plug-in/xreq.xml      $(DESTDIR)$(GPSDATADIR)/library/xreq.xml
	$(INSTALL) -m644 data/gps-plug-in/xreq.py       $(DESTDIR)$(GPSDATADIR)/library/xreq.py
	$(INSTALL) -m644 data/gps-plug-in/feature-lang.xml $(DESTDIR)$(GPSDATADIR)/plug-ins/feature-lang.xml
	$(INSTALL) -m755 lib/gps/libxreqgps.$(SUF_SO)          $(DESTDIR)$(LIBDIR)/libxreqgps.$(SUF_SO)
endif

install-gpr: data/gprconfig.xml
	mkdir -p $(DESTDIR)$(DATADIR)/gprconfig
	$(INSTALL) -m644 data/gprconfig.xml $(DESTDIR)$(DATADIR)/gprconfig/xreq.xml

uninstall: uninstall-gps
	-$(RM) -rf $(DESTDIR)$(BINDIR)/xreq
	-$(RM) -rf $(DESTDIR)$(GPRDIR)/xreqlib.gpr
	-$(RM) -rf $(DESTDIR)$(INCLUDEDIR)/xreqlib
	-$(RM) -rf $(DESTDIR)$(LIBDIR)/xreqlib
	-$(RM) -rf $(DESTDIR)$(LIBDIR)/libxreq.$(SUF_SO)
	-$(RM) -rf $(DESTDIR)$(INCLUDEDIR)/xreq.h
	-$(RM) -rf $(DESTDIR)$(DOCDIR)
	-$(RM) -rf $(DESTDIR)$(DATADIR)/XReq

uninstall-gps:
	-$(RM) -rf $(DESTDIR)$(LIBDIR)/libxreqgps.$(SUF_SO)
ifneq ($(GPSDATADIR),)
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/plug-ins/xreq.xml
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/plug-ins/xreq.py
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/plug-ins/feature-lang.xml
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/library/xreq.xml
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/library/xreq.py
	-$(RM) -rf $(DESTDIR)$(GPSDATADIR)/library/feature-lang.xml
endif

uninstall-gpr: data/gprconfig.xml
	-$(RM) -rf $(DESTDIR)$(DATADIR)/gprconfig/xreq.xml

install-gps-local:
	ln -sf "`pwd`"/data/gps-plug-in/*.{xml,py} ~/.gps/plug-ins
	ln -sf "`pwd`"/lib/gps/libxreqgps.$(SUF_SO) ~/.local/lib

uninstall-gps-local:
	-$(RM) ~/.gps/plug-ins/xreq.xml
	-$(RM) ~/.gps/plug-ins/xreq.py
	-$(RM) ~/.gps/plug-ins/feature-lang.xml
	-$(RM) ~/.local/lib/libxreqgps.$(SUF_SO)

.PHONY: install install-lib install-bin install-gps uninstall install-gps-local uninstall-gps uninstall-gps-local install-gpr uninstall-gpr



###################
##               ##
##    ARCHIVE    ##
##               ##
###################


archive: $(ARCHIVENAME).tar.bz2
	@echo "Created $+"

# TODO: include $(DOC_FILES)
$(ARCHIVENAME).tar:
	@echo "GIT     ARCHIVE $@"
	git archive --prefix=$(ARCHIVENAME)/ -o $@ HEAD

$(ARCHIVENAME).tar.bz2: $(ARCHIVENAME).tar
	@echo "BZIP2   $@"
	bzip2 --best $<

.PHONY: archive

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
	@echo "TARGETS:"
	@echo
	@echo "Building:"
	@echo "    all:            Build everything     [bin gps-plugin tests doc]"
	@echo "    build:          Build everything for installation"
	@echo "                                         [bin gps-plugin]"
	@echo "    bin:            Build XReq           [bin/xreq]"
	@echo "    bin:            Build XReq libraries [lib]"
	@echo "    gps-plugin:     Build GPS plugin     [lib/gps]"
	@echo "    tests:          Build tests          [bin/*_tests]"
	@echo "    doc:            Build documentation  [README.html]"
	@echo "Checking:"
	@echo "    coverage:       Run coverage tests   [coverage/]"
	@echo "    cov-help:       Help on coverage"
	@echo "    gnatcheck:      Run gnatcheck        [reports/gnatcheck*]"
	@echo "    report-all:     Create HTML reports  [reports/*.html]"
	@echo "    check-tests:    Check tests passed"
	@echo "Testing (interactive use):"
	@echo "    run-unit:       Run all unit tests"
	@echo "    run-xreq:       Run all acceptance tests with XReq"
	@echo "    run-cucumber:   Run all acceptance tests with Cucumber"
	@echo "    rerun-cucumber: Run last failed cucumber tests"
	@echo "    run-features:   Run all cucumber and XReq tests"
	@echo "Other:"
	@echo "    clean:          Clean project"
	@echo "    archive:        Create a source archive   [$(ARCHIVENAME).tar.bz2]"
	@echo "    install:        Install XReq"
	@echo "    uninstall:      Uninstall XReq"
	@echo "    install-gps-local:"
	@echo "                    Install the GPS plugin in your HOME directory"
	@echo "                    using symbolic links rather than copying files"
	@echo "    show-ignored-coverage:"
	@echo "                    Show lines that are ignored by gcov"
	@echo
	@echo "VARIABLES:"
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
	@echo
	@echo "HOW TO INSTALL"
	@echo
	@echo "    $ make build   (as normal user)"
	@echo "    # make install (as priviledged user)"

.PHONY: help show-ignored-coverage

src/lib/xreqlib-format_html_template.ads src/lib/xreqlib-format_html_template.adb: src/xreq-report.template.html tools/template.pl
	$(REDO) $@

####################
##                ##
##    MARKDOWN    ##
##                ##
####################
MARKDOWN_CMDLINE=tools/Markdown.pl <$< >$@

tools/Markdown.pl:
	$(REDO) tools/Markdown.pl

%.html: %.mdwn tools/Markdown.pl
	@echo "MDWN    $@"
	$(MARKDOWN_CMDLINE)

%.html: %.md tools/Markdown.pl
	@echo "MDWN    $@"
	$(MARKDOWN_CMDLINE)

%.html: % tools/Markdown.pl
	@echo "MDWN    $@"
	$(MARKDOWN_CMDLINE)
