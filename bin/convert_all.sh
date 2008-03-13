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
# update
_svn t update

cd        ${OT_EDITHOME}/edit/
mkdir -p  ${OT_EDITHOME}/_out
mkdir -p  ${OT_EDITHOME}/_tmp


/bin/ls -1 *.ot > ../_tmp/all.list
_svn t ls > ../_tmp/svn.list

filelist=`cat ../_tmp/all.list ../_tmp/svn.list | sort | uniq -d`
locallist=`cat ../_tmp/all.list ../_tmp/svn.list | sort | uniq -u`
_svn t log --xml "..@HEAD" > ../_tmp/tmp.log

for f in $locallist;
do
  base=`basename ${f} .ot`
  echo "[" ${base} "]"
  _oldtype_to internal "${base}.ot" ../_out/${base}.sexp
done

for f in $filelist;
do
  base=`basename ${f} .ot`
  if [ "${base}.ot" -nt ../_out/${base}.sexp ] ; then
      echo "[" ${base} "]"
      _svn t ann "${f}@HEAD" > ../_tmp/tmp.ann
      if [ "$?" != "0" ] ; then
	  msg="Warning: [ ${f} ] svn ann  command failed..."
	  echo ${msg}
	  logger "OldType: ${msg}"
      fi
      _oldtype_to internal "${base}.ot" ../_out/${base}.sexp     ../_tmp/tmp.log ../_tmp/tmp.ann
  fi
done

# commit

if [ "${OT_MASTER}" = "t" ] ; then
    _svn t commit -m backend_process
fi

popd