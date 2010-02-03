##                         Copyright (C) 2010, Sogilis                       ##

GNATMAKE=gnatmake
TEST_SUITES=test coverage

all: bin test doc

dir:
	mkdir -p obj
	mkdir -p bin
	mkdir -p doc
	mkdir -p reports

bin: dir
	$(GNATMAKE) -P main.gpr

test: dir
	$(GNATMAKE) -P tests.gpr

doc: dir README.html src/README.html
	
clean:
	-$(RM) obj/*
	-$(RM) bin/*
	-$(RM) README.html
	-$(RM) src/README.html
	-$(RM) reports/*.gcov
	-$(RM) reports/gcov.summary.txt
	-$(RM) reports/gnatcheck.out
	-$(RM) reports/*.aunit.gcov

gcov-reset: dir
	-$(RM) reports/*.gcov
	-$(RM) reports/gcov.summary.txt
	-$(RM) obj/*.gcda

gcov:
	cd reports && gcov -f -o ../obj ../src/*.adb | tee gcov.summary.txt

coverage: test
	$(MAKE) gcov-reset
	bin/tests -xml >reports/test.aunit.xml
	$(MAKE) gcov

check: dir
	cd reports && gnat check -P ../main.gpr -rules -from=../gnatcheck.rules

test-report: dir test
	for t in $(TEST_SUITES); do \
	  bin/tests -xml -suite="$$t" >"reports/$$t.aunit.xml"; \
	done

.PHONY: all dir bin test doc clean gcov-reset gcov coverage check test-report





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
