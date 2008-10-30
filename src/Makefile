# -*- coding: utf-8 -*-

OTSAMPLE='../edit/Test.ot'

all: log.txt  Test.ann.txt  Entry1.ann.txt  Entry2.ann.txt
	gosh -I ../Kahua/oldtype ./oldtype_to internal  ../edit/Test.ot    log.txt Test.ann.txt    > Test.sexp
	gosh -I ../Kahua/oldtype ./oldtype_to internal  ../edit/Test.ot                            > Test.no-timeline.sexp
	gosh -I ../Kahua/oldtype ./oldtype_to internal  ../edit/Entry1.ot  log.txt Entry1.ann.txt  > Entry1.sexp
	gosh -I ../Kahua/oldtype ./oldtype_to internal  ../edit/Entry2.ot  log.txt Entry2.ann.txt  > Entry2.sexp
	gosh -I ../Kahua/oldtype ./oldtype_to sxml      ../edit/Sample1.ot > Sample1.sxml
	gosh -I ../Kahua/oldtype ./oldtype_to sxml      ../edit/Sample2.ot > Sample2.sxml
	gosh -I ../Kahua/oldtype ./oldtype_to plain     ../edit/Sample1.ot > Sample1.txt
	gosh -I ../Kahua/oldtype ./oldtype_to plain     ../edit/Sample2.ot > Sample2.txt
	gosh -I ../Kahua/oldtype ./oldtype_to rich      ../edit/Sample1.ot > Sample1.rich.txt
	gosh -I ../Kahua/oldtype ./oldtype_to rich      ../edit/Sample2.ot > Sample2.rich.txt
	gosh -I ../Kahua/oldtype ./oldtype_to commands  ../edit/Sample1.ot > Sample1.commands
	gosh -I ../Kahua/oldtype ./oldtype_to commands  ../edit/Sample2.ot > Sample2.commands

test: log.txt Test.ann.txt
	gosh -I ../Kahua/oldtype test.scm  ${OTSAMPLE} log.txt Test.ann.txt > test.log

log.txt: ../edit/Test.ot ../edit/Entry1.ot ../edit/Entry2.ot
	svn --xml log ../ > log.txt

Test.ann.txt:   ../edit/Test.ot
	svn ann ../edit/Test.ot    > Test.ann.txt

Entry1.ann.txt: ../edit/Entry1.ot
	svn ann ../edit/Entry1.ot  > Entry1.ann.txt

Entry2.ann.txt: ../edit/Entry2.ot
	svn ann ../edit/Entry2.ot  > Entry2.ann.txt


svninfo:
	svn --xml info ${OTSAMPLE} > info.xml
	gosh -I ../Kahua/oldtype ./oldtype_svninfo shell < info.xml
