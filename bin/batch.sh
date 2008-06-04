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
  ${OT_HOME}/bin/update.sh &&
  ${OT_HOME}/bin/convert.sh &&
  ${OT_HOME}/bin/ot_AllPages &&
  ${OT_HOME}/bin/ot_RecentChanges &&
  ${OT_HOME}/command/blog &&
  ${OT_HOME}/bin/add.sh &&
  ${OT_HOME}/bin/commit.sh
  #run-parts ${OT_HOME}/hook
  sleep 5
}

while :
do
  task
done


