
LEX=flex

CC=g++

CPPFLAGS=-Wall -Werror -g -std=c++11
LDLIBS=-lfl  

targets=parseGrammar split c.parse c.generate.xsl xml.parse xml.generate.xsl

all: ${targets}

c.parse: tokenStreamer.h

%.cc: %.ll %.tab.cc
	flex -t $< > $@

%.tab.cc: %.yy
	bison $*.yy

clean:
	rm ${targets}

%.spec.xml: %.spec
	cat $< | ./parseGrammar > $@

%.parse.yy: %.spec.xml parse.xsl
	cat $< | ./split | xmlstarlet tr parse.xsl > $@

%.parse.ll: %.spec
	cat parseGrammar.ll | perl -pe "s/parseGrammar/$*.parse/;" > $@

%.generate.xsl: %.spec.xml
	cat $< | xmlstarlet tr generate.xsl > $@

