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

  prev=`_svn t info   | grep Revision: | awk '{ print $2; }'`
  _svn t update
  curr=`_svn t info   | grep Revision: | awk '{ print $2; }'`

popd

if [ "$1" = "1" ] ; then
  echo "[[info]] force update"
  # force option
  status=0
else
  echo "[[info]] revision[$prev]=>[$curr]"
  [ "$prev" != "$curr" ]
  status=$?
fi
exit $status
