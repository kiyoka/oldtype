# -*- coding: utf-8 -*-

OTSAMPLE='../edit/Test.ot'

all: log.txt ann.txt
	gosh -I ../Kahua/oldtype ./oldtype_to internal  ${OTSAMPLE} log.txt ann.txt > out.sexp
	gosh -I ../Kahua/oldtype ./oldtype_to sxml      ${OTSAMPLE} > out.sxml
	gosh -I ../Kahua/oldtype ./oldtype_to plain     ${OTSAMPLE} > out.txt

test: log.txt ann.txt
	gosh -I ../Kahua/oldtype test.scm  ${OTSAMPLE} log.txt ann.txt > test.log

log.txt:
	svn --xml log ../ > log.txt

ann.txt:
	svn ann ${OTSAMPLE} > ann.txt


svninfo:
	svn --xml info ${OTSAMPLE} > info.xml
	gosh -I ../Kahua/oldtype ./oldtype_svninfo shell < info.xml
