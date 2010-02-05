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

gcov: bin
	cd reports && gcov -o ../obj ../src/*.adb > gcov.summary.txt
	for gcov in reports/*.gcov; do \
		base="`basename "$$gcov"`"; \
		if [ ! -e "src/$${base%.gcov}" ]; then \
			rm "$$gcov"; \
		fi; \
	done
	bin/tests -suite=coverage -text

coverage: test bin
	$(MAKE) gcov-reset
	bin/tests >/dev/null 2>/dev/null
	$(MAKE) gcov

gnatcheck: dir
	cd reports && gnat check -P ../tests.gpr -rules -from=../gnatcheck.rules

test-report: dir bin test
	for t in $(TEST_SUITES); do \
	  echo "========== RUN TEST SUITE $$t =========="; \
	  echo bin/tests -xml -suite="$$t" -o"reports/$$t.aunit.xml"; \
	  bin/tests -xml -suite="$$t" -o"reports/$$t.aunit.xml"; \
	  cat "reports/$$t.aunit.xml"; \
	done

clean-reports: gcov-reset
	-$(RM) reports/gnatcheck.out
	-$(RM) reports/*.aunit.gcov

check: bin clean-reports coverage gnatcheck
	for t in $(TEST_SUITES); do \
	  echo bin/tests -suite="$$t"; \
	  bin/tests -suite="$$t"; \
	done

.PHONY: all dir bin test doc clean clean-reports gcov-reset gcov coverage gnatcheck check test-report





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
