#!/bin/bash

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
