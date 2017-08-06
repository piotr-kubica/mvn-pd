#!/usr/bin/env bash
if [ -z "$1" ] 
then
  echo "[$0] No source path parameted supplied."
  exit 1
fi
if [ -z "$2" ] 
then
  echo "[$0] No destination path parameter supplied."
  exit 1
fi
i=1
random="pom"
find $1 -name 'pom*' -type f | while read f
do
  newbase=${f/*./$random$i.}
  echo "[$0] Copying file $f to $2/$newbase"
  cp $f "$2/$newbase"
  ((i++))
done
