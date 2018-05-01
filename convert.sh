#!/bin/bash

for i in *.xlsx ; do
 echo "Processing $i."
 out=$(basename "${i}" .xlsx)
 out=`echo ${out} | sed s/\ /_/ | sed -e 's/\(.*\)/\L\1/'`
 echo "Writing to $out."
 xlsx2csv --all "${i}" "${out}"
 echo "Done."
done
