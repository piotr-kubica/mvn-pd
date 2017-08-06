#!/usr/bin/env bash
source="source"
sourcenes="d2"
dest="dest"
pomafile="pom-a.xml"
pombfile="pom-b.xml"

$(mkdir "$source")
$(mkdir "$source/$sourcenes")
$(mkdir "$dest")
$(touch "$source/$pomafile")
$(touch "$source/$sourcenes/$pombfile")

bash find-copy-rename-pom.sh "$source" "$dest"

if [ -f "$dest/pom1.xml" ] && [ -f "$dest/pom2.xml" ]; then
   echo "Test passed. Pom files exist"
else
   echo "Test failed! Pom files does not exist!"
fi
