
LEX=flex

CC=g++

LDLIBS=-lfl  

targets=parseGrammar split c.parse c.generate.xsl

all: ${targets}

%.cc: %.ll %.tab.cc
	flex -t $< > $@

%.tab.cc: %.yy
	bison $*.yy

clean:
	rm ${targets}

%.parse.yy: %.spec
	cat $< | ./parseGrammar | ./split | xmlstarlet tr parse.xsl > $@

%.parse.ll: %.spec
	cat parseGrammar.ll | perl -pe "s/parseGrammar/$*.parse/;" > $@

%.generate.xsl: %.spec
	cat $< | ./parseGrammar | xmlstarlet tr generate.xsl > $@

