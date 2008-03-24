#!/bin/bash

if [ "$OT_SITE" = "" ] ; then
  echo "Please environment variables OT_SITE"
  exit 1
fi

export OT_EDITHOME=${OT_SITE}/tmp/oldtype
export OT_STATICHOME=${OT_SITE}/static/oldtype

_svn() {
    exit_on_error=$1 ; shift
    args=$*
    exec_string="svn ${args} --username ${OT_USER_BACKEND} --password ${OT_PASS_BACKEND} --no-auth-cache"
    ## echo     ${exec_string} 1>&2
    ${exec_string}
    status=$?
    if [ "${status}" != "0" -a "${exit_on_error}" = "t" ] ; then
	echo "Error: When execute string '${exec_string}'" 1>&2
	exit ${status}
    fi
    return ${status}
}
