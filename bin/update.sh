#!/bin/bash

#load configuration
. ${OT_HOME}/config.sh
. ${OT_HOME}/bin/lib.sh

_oldtype_to() {
    t=$1
    src=$2
    dst=$3
    log=$4
    ann=$5
    ${OT_HOME}/src/oldtype_to ${t} ${src} ${log} ${ann} > ../_out/${base}.sexp.tmp

    if [ "$?" = "0" ] ; then
	/bin/mv -f ../_out/${base}.sexp.tmp ../_out/${base}.sexp
    else
	exit $?
    fi
}

pushd . >& /dev/null

cd        ${OT_EDITHOME}

  _svn t update

popd
