##                         Copyright (C) 2010, Sogilis                       ##

GNATMAKE=gnatmake

all: bin test doc

dir:
	mkdir -p obj
	mkdir -p bin

bin: dir
	$(GNATMAKE) -P main.gpr

test: dir
	$(GNATMAKE) -P tests.gpr

doc: README.html src/README.html
	
clean:
	-$(RM) obj/*
	-$(RM) bin/*
	-$(RM) README.html
	-$(RM) src/README.html


.PHONY: all dir bin test doc clean





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
