#!/bin/bash

#load configuration
. ${OT_HOME}/config.sh
. ${OT_HOME}/bin/lib.sh

pushd . >& /dev/null
cd        ${OT_EDITHOME}

# add
#if [ "${OT_MASTER}" = "t" ] ; then
#    _svn t commit -m backend_process
#fi

popd
