#!/bin/bash

#load configuration
. ${OT_HOME}/config.sh
. ${OT_HOME}/bin/lib.sh

pushd . >& /dev/null
cd        ${OT_EDITHOME}/edit/

generated=`/bin/ls -1 \!*.ot`
for f in ${generated};
do
  hatena=`_svn t status ${f} | awk '{ print $1; }'`
  if [ "?" = "${hatena}" ] ; then
     echo "Added file " ${f}
     svn add ${f}
  fi
done

popd
