##                         Copyright (C) 2010, Sogilis                       ##

GNATMAKE=gnatmake
TEST_SUITES=test coverage
CONFIG=dbg
MODE=
LIBTYPE=dynamic
LIBEXT=

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

GPRBUILD=gprbuild
GPS=gps
INSTALL=install
CP=cp -R
MKDIR=mkdir -p

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

lib: lib/$(MODE)/libxreqlib.$(LIBEXT)

lib/debug/libxreqlib.so: dir
	$(GPRBUILD) -Pxreqlib.gpr -Xtype=dynamic -Xmode=debug

lib/release/libxreqlib.so: dir
	$(GPRBUILD) -Pxreqlib.gpr -Xtype=dynamic -Xmode=release

lib/coverage/libxreqlib.so: dir
	$(GPRBUILD) -Pxreqlib.gpr -Xtype=dynamic -Xmode=coverage

lib/debug/libxreqlib.a: dir
	$(GPRBUILD) -Pxreqlib.gpr -Xtype=static  -Xmode=debug

lib/release/libxreqlib.a: dir
	$(GPRBUILD) -Pxreqlib.gpr -Xtype=static  -Xmode=release

lib/coverage/libxreqlib.a: dir
	$(GPRBUILD) -Pxreqlib.gpr -Xtype=static  -Xmode=coverage

bin/xreq.cov: dir
	# GPRBuild is a really broken piece of software. It seems that there is
	# no way to tell the compiler to static-link a specific library. So, we
	# have ro remove it beforehand to make sure we use the dynamic library.
	-mv lib/coverage/libxreqlib.so lib/coverage/libxreqlib.so-
	$(GPRBUILD) -Pxreq.gpr    -Xtype=static -Xmode=coverage
	-mv lib/coverage/libxreqlib.so- lib/coverage/libxreqlib.so

bin/xreq.rel: dir
	# GPRBuild is a really broken piece of software. It seems that there is
	# no way to tell the compiler to static-link a specific library. So, we
	# have ro remove it beforehand to make sure we use the dynamic library.
	-mv lib/release/libxreqlib.so lib/release/libxreqlib.so-
	$(GPRBUILD) -Pxreq.gpr    -Xtype=static -Xmode=release
	-mv lib/release/libxreqlib.so- lib/release/libxreqlib.so

bin/xreq.dbg: dir
	# GPRBuild is a really broken piece of software. It seems that there is
	# no way to tell the compiler to static-link a specific library. So, we
	# have ro remove it beforehand to make sure we use the dynamic library.
	-mv lib/debug/libxreqlib.so lib/debug/libxreqlib.so-
	$(GPRBUILD) -Pxreq.gpr    -Xtype=static -Xmode=debug
	-mv lib/debug/libxreqlib.so- lib/debug/libxreqlib.so

bin/xreq: bin/xreq.$(CONFIG)
	-rm -f bin/xreq
	ln -s xreq.$(CONFIG) bin/xreq

lib/gps/libxreqgps.so: dir
	$(GPRBUILD) -Pgps_plugin.gpr -Xmode=release
	#$(MAKE) -C src/gps libgprcustom.so && mv src/gps/libgprcustom.so $@

gps-plugin: lib/gps/libxreqgps.so

bin/unit_tests: dir
	$(GPRBUILD) -Punit_tests.gpr -Xmode=debug

bin/unit_tests.cov: dir
	$(GPRBUILD) -Punit_tests.gpr -Xmode=coverage

bootstrap: bin/feature_tests

bin/feature_tests: bin/feature_tests.dbg
	ln -s -f feature_tests.dbg $@

bin/feature_tests.dbg: bin/xreq features/*.feature
	-$(MKDIR) features/tests/dbg
	GNAT_FLAGS="-g -gnata -E" \
	bin/xreq -m -x feature_tests -o features/tests/dbg features/*.feature
	$(CP) features/tests/dbg/feature_tests $@

bin/feature_tests.cov: bin/xreq features/*.feature
	-$(MKDIR) features/tests/cov
	GNAT_FLAGS="-g -gnata -E -fprofile-arcs -ftest-coverage" \
	bin/xreq -m -x feature_tests -o features/tests/cov features/*.feature
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

.PHONY: all dir lib bin gps-plugin tests bootstrap doc clean



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

coverage: tests bin/xreq.cov bin/unit_tests.cov bin/feature_tests.cov
	@echo
	@echo "##########################"
	@echo "##  Run coverage tests  ##"
	@echo "##########################"
	@echo
	$(GPRBUILD) -P xreq.gpr -Xmode=coverage
	-$(RM) -f bin/xreq
	$(MAKE) clean-gcov
	lcov -q -c -i -d obj/coverage -t "Ignored_Lines" -o coverage/2-ignore.lcov.info.tmp
	perl gcov-ignore.pl coverage/2-ignore.lcov.info.tmp > coverage/2-ignore.lcov.info
	ln -s xreq.cov bin/xreq
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
	-make -C features/tests clean
	-\
	GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
	COVERAGE="`pwd`/coverage/cuke" COV_OBJ_DIR="`pwd`/obj/coverage" mode=coverage \
	XREQ_BEFORE_MAKE_SILENT=true \
	XREQ_BEFORE_MAKE='if [ -n "$$COVERAGE" ]; then n=0; while [ -e "$$COVERAGE/$$n.lcov.info" ]; do let n=n+1; done; f="$$COVERAGE/$$n.lcov.info"; echo "Create: $$f"; lcov -q -c -d "$$COV_OBJ_DIR" -t XReq -o "$$f"; lcov -q -d "$$COV_OBJ_DIR" --zerocounters; if ! [ -s "$$f" ]; then rm "$$f"; fi; fi' \
	bin/feature_tests.cov -d -t ~@bootstrap+~@wip -f html -o reports/features-xreq.html
	-\
	GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
	COVERAGE="`pwd`/coverage/cuke" COV_OBJ_DIR="`pwd`/obj/coverage" mode=coverage \
	XREQ_BEFORE_MAKE_SILENT=true \
	XREQ_BEFORE_MAKE='if [ -n "$$COVERAGE" ]; then n=0; while [ -e "$$COVERAGE/$$n.lcov.info" ]; do let n=n+1; done; f="$$COVERAGE/$$n.lcov.info"; echo "Create: $$f"; lcov -q -c -d "$$COV_OBJ_DIR" -t XReq -o "$$f"; lcov -q -d "$$COV_OBJ_DIR" --zerocounters; if ! [ -s "$$f" ]; then rm "$$f"; fi; fi' \
	bin/feature_tests.cov -d -t @wip -f html -o reports/features-wip-xreq.html
# #	-\
# #	GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
# #	COVERAGE="`pwd`/coverage/cuke" COV_OBJ_DIR="`pwd`/obj/coverage" mode=coverage \
# #	cucumber -t "@bootstrap" features/*.feature
# 	-\
# 	GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
# 	COVERAGE="`pwd`/coverage/cuke" COV_OBJ_DIR="`pwd`/obj/coverage" mode=coverage \
# 	cucumber -t "~@wip" -t "~@bootstrap" -f html  -o reports/features.html      features/*.feature >/dev/null 2>&1
# 	-\
# 	GNAT_FLAGS="-ftest-coverage -fprofile-arcs -g" \
# 	COVERAGE="`pwd`/coverage/cuke" COV_OBJ_DIR="`pwd`/obj/coverage" mode=coverage \
# 	cucumber -w -t "@wip" -f html  -o reports/features-wip.html  features/*.feature >/dev/null 2>&1
# 	lcov -q -c -d obj/coverage -t "Cucumber" -o coverage/cuke/last.lcov.info
	-$(RM) -f bin/xreq
	ln -s xreq.$(CONFIG) bin/xreq
	lcov -q -c -d "obj/coverage" -t XReq -o "coverage/cuke/last.lcov.info"
	$(MAKE) _gcov-gather-cucumber
	$(MAKE) gcov-report

_gcov-gather-cucumber:
	lcov $(foreach lcov,$(wildcard coverage/cuke/*.lcov.info),-a $(lcov)) -o coverage/11-cucumber.lcov.info
	-rm -rf coverage/cuke

.PHONY: clean-gcov gcov-report _gcov-gather-cucumber coverage

_cucumber_clean_rerun:
	-$(RM) -f cucumber-rerun.txt

run-cucumber: bin
	$(MAKE) run-cucumber-wip
	$(MAKE) run-cucumber-ada
	$(MAKE) run-cucumber-c

run-cucumber-wip: bin _cucumber_clean_rerun
	@echo
	@echo "#########################################"
	@echo "##  Run cucumber for work in progress  ##"
	@echo "#########################################"
	cucumber -w -t "@wip" -f progress features/*.feature

run-cucumber-c: bin _cucumber_clean_rerun
	@echo
	@echo "##########################"
	@echo "##  Run cucumber for C  ##"
	@echo "##########################"
	@echo
	XREQ_LANG=C \
	cucumber -t "~@wip" -t "~@bootstrap" -t "@lang-Ada,~@lang" -f progress -f rerun -o cucumber-rerun.txt features/*.feature

run-cucumber-ada: bin _cucumber_clean_rerun
	@echo
	@echo "############################"
	@echo "##  Run cucumber for Ada  ##"
	@echo "############################"
	@echo
	XREQ_LANG=Ada \
	cucumber -t "~@wip" -t "~@bootstrap" -t "@lang-Ada,~@lang" -f progress -f rerun -o cucumber-rerun.txt features/*.feature

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

run-xreq: bin
	@echo
	@echo "###################"
	@echo "##  Run XReq  ##"
	@echo "###################"
	@echo
	bin/xreq -x suite features/*.feature
	$(GPRBUILD) -Pfeatures/tests/suite.gpr
	features/tests/suite

run-tests: tests bin
	@echo
	@echo "######################"
	@echo "##  Run unit tests  ##"
	@echo "######################"
	@echo
	bin/unit_tests

.PHONY: run-cucumber rerun-cucumber run-cucumber-wip run-cucumber-c run-cucumber-ada _cucumber_clean_rerun run-tests

gnatcheck: dir
	@echo
	@echo "######################"
	@echo "##  Run GNAT-Check  ##"
	@echo "######################"
	@echo
	cd reports && gnat check -P../xreq.gpr -U -rules -from=../gnatcheck.rules
	cd reports && mv gnatcheck.out gnatcheck.xreq.out
	#cd reports && gnat check -P../xreqlib.gpr -U -rules -from=../gnatcheck.rules
	#cd reports && mv gnatcheck.out gnatcheck.xreqlib.out
	#cd reports && gnat check -P../unit_tests.gpr -rules -from=../gnatcheck.rules
	#cd reports && mv gnatcheck.out gnatcheck.tests.out

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

features/tests/suite.gpr: bin/xreq $(wildcard features/*.feature)
	bin/xreq -x suite $(wildcard features/*.feature)

features/tests/suite: features/tests/suite.gpr
	$(GPRBUILD) -Pfeatures/tests/suite.gpr

reports/features-xreq.html: features/tests/suite
	-features/tests/suite -f html -o reports/features-xreq.html

check: gnatcheck coverage run-cucumber run-tests

.PHONY: gnatcheck test-report test-report-cucumber test-report-unit check

install: bin/xreq.rel install-gps
	$(INSTALL) -D bin/xreq.rel $(DESTDIR)$(BINDIR)/xreq
	$(INSTALL) -m644 -D data/xreqlib.gpr $(DESTDIR)$(GPRDIR)/xreqlib.gpr
	$(INSTALL) -d $(DESTDIR)$(INCLUDEDIR)/xreqlib
	$(CP) src/lib/*.ad[bs] $(DESTDIR)$(INCLUDEDIR)/xreqlib
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)/xreqlib
	$(CP) lib/release/* $(DESTDIR)$(LIBDIR)/xreqlib
	@echo '------------------------------------------------------------------'
	@echo '--  XReq has now been installed.'
	@echo '------------------------------------------------------------------'
	@echo '--  To be able to use the xreq binary, you may need to update'
	@echo '--  your PATH to point to'
	@echo '--  $(DESTDIR)$(BINDIR)'
	@echo '------------------------------------------------------------------'
	@echo '--  To be able to use the library, you may need to update your'
	@echo '--  ADA_PROJECT_PATH or GPR_PROJECT_PATH to point to the path'
	@echo '--  $(DESTDIR)$(GPRDIR)'
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

show-ignored-coverage:
	find src -name "*.ad[bs]" -print0 | xargs -0 grep -Rn GCOV_IGNORE

help:
	@echo "make TARGET"
	@echo
	@echo "Targets:"
	@echo
	@echo "    all:            Build everything    [bin gps-plugin tests doc]"
	@echo "    bin:            Build project       [bin/xreq]"
	@echo "    gps-plugin:     Build GPS plugin    [lib/gps]"
	@echo "    tests:          Build tests         [bin/unit_tests]"
	@echo "    doc:            Build documentation [README.html]"
	@echo "    coverage:       Run coverage tests  [coverage/]"
	@echo "    gnatcheck:      Run gnatcheck       [reports/gnatcheck*]"
	@echo "    test-report:    Create test reports [reports/]"
	@echo "    run-tests:      Run all unit tests"
	@echo "    run-cucumber:   Run all cucumber tests"
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
	@echo "    INCLUDEDIR      ???                       [$(INCLUDEDIR)]"
	@echo "    LIBDIR          ???                       [$(LIBDIR)]"
	@echo "    GPRDIR          GNAT Project files        [$(GPRDIR)]"
	@echo "    DOCDIR          Documentation directory   [$(DOCDIR)]"
	@echo "    DATADIR         Read only architecture-independant data"
	@echo "                                              [$(DATADIR)]"
	@echo "    GPSDATADIR      GPS plugin data           [$(GPSDATADIR)]"

.PHONY: help show-ignored-coverage

src/lib/xreqlib-format_html_template.ads src/lib/xreqlib-format_html_template.adb: src/xreq-report.template.html ./template.pl
	./template.pl $< XReqLib.Format_HTML_Template $@

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
