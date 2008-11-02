#!/bin/bash

#load configuration
. ${OT_HOME}/config.sh
. ${OT_HOME}/bin/lib.sh

_oldtype_to() {
    src=$1
    log=$2
    ann=$3
    ${OT_HOME}/src/oldtype_to internal ${src} ${log} ${ann} > ../_out/${base}.sexp.tmp
    ${OT_HOME}/src/oldtype_to commands ${src} ${log} ${ann} > ../_out/${base}.commands

    if [ "$?" = "0" ] ; then
	/bin/mv -f ../_out/${base}.sexp.tmp ../_out/${base}.sexp
        /bin/cp -f ${src} ../_out/${base}.ot
    else
	exit $?
    fi
}

pushd . >& /dev/null

cd        ${OT_EDITHOME}/edit/
mkdir -p  ${OT_EDITHOME}/_out
mkdir -p  ${OT_EDITHOME}/_tmp

# copy images
mkdir -p  ${OT_STATICHOME}/img
/bin/cp -f ${OT_EDITHOME}/img/* ${OT_STATICHOME}/img

/bin/ls -1 *.ot > ../_tmp/all.list
_svn t ls > ../_tmp/svn.list

filelist=`cat ../_tmp/all.list ../_tmp/svn.list | sort | uniq -d`
locallist=`cat ../_tmp/all.list ../_tmp/svn.list | sort | uniq -u`
_svn t log --xml "..@HEAD" > ../_tmp/tmp.log

for f in $locallist;
do
  base=`basename ${f} .ot`
  echo "[" ${base} "]"
  _oldtype_to "${base}.ot"
done

function convert_p() {
  base=$1
  status=1
  if [ ! -f "../_out/${base}.ot" ] ; then
    # doesn't exist yet
    status=0
  else
    diff "${base}.ot" "../_out/${base}.ot" > /dev/null
    if [ "$?" != "0" ] ; then
      # difference exist
      status=0
    fi     
  fi
  return $status
}


for f in $filelist;
do
  base=`basename ${f} .ot`
  convert_p ${base}
  if [ "0" = "$?" ] ; then
      echo "[" ${base} "]"
      _svn t ann "${f}@HEAD" > ../_tmp/tmp.ann
      if [ "$?" != "0" ] ; then
	  msg="Warning: [ ${f} ] svn ann  command failed..."
	  echo ${msg}
	  logger "OldType: ${msg}"
      fi
      _oldtype_to "${base}.ot"   ../_tmp/tmp.log ../_tmp/tmp.ann
  fi
done

# create _commands.sexp
echo "(" > ../_out/__commands.sexp
cat ../_out/*.commands >> ../_out/__commands.sexp
echo ")" >> ../_out/__commands.sexp

popd
