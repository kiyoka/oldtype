#!/bin/bash


if [ "$OT_HOME" = "" ] ; then
  echo "Please environment variables OT_HOME"
  exit 1
fi

# setup load-path
export GAUCHE_LOAD_PATH="$OT_HOME/Kahua/oldtype"

# load configuration
. ${OT_HOME}/config.sh

if [ "$OT_SITE" = "" ] ; then
  echo "Please environment variables OT_SITE"
  exit 1
fi

if [ "$OT_USER_BACKEND" = "" ] ; then
  echo "Please environment variables OT_USER_BACKEND"
  exit 1
fi

if [ "$OT_USER_LOCAL" = "" ] ; then
  echo "Please environment variables OT_USER_LOCAL"
  exit 1
fi

function task () {
  force=$1
  echo [[info]] task start
  ${OT_HOME}/bin/update.sh ${force} &&
  ${OT_HOME}/bin/ot_AllPages &&
  ${OT_HOME}/bin/ot_RecentChanges &&
  ${OT_HOME}/command/blog &&
  ${OT_HOME}/bin/add.sh &&
  ${OT_HOME}/bin/commit.sh &&
  ${OT_HOME}/bin/update.sh ${force} &&
  ${OT_HOME}/bin/convert.sh
  #run-parts ${OT_HOME}/hook
  echo -n [[info]] sleep...
  sleep 2
  echo wakeup
}

cnt=0
while :
do
  if [ "${cnt}" = "0" ] ; then
    task 1
  else
    task 0
  fi

  cnt=`expr ${cnt} + 1`
  if [ "${cnt}" -gt "10" ] ; then
    cnt=0
  fi
done
