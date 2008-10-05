#!/bin/zsh

#cat tdiary_data/**/*.td2 | iconv -f euc-jp -t utf8 > td2.txt
#cat tdiary_data/**/*.tdc | iconv -f euc-jp -t utf8 > tdc.txt

gosh ./convertToSexp.scm d < td2.txt > td2.scm
gosh ./convertToSexp.scm c < tdc.txt > tdc.scm

#gosh ./convertToSexp.scm d < td2_small.txt > td2.scm
#gosh ./convertToSexp.scm c < tdc_small.txt > tdc.scm

mkdir -p out
gosh ./convertToOT.scm ${USER} td2.scm tdc.scm
