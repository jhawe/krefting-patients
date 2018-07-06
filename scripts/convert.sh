#!/bin/bash

# NOTE
# use of this script is deprecated!
# since we implemented full excel file processing
# in the R script

echo starting at `date +%Y-%m-%d-%H-%M-%S`

echo "Input data in: $1"
echo "Output data in: $2"
for i in $1/*.xlsx ; do
 echo "Processing $i."
 out=$2/$(basename "${i}" .xlsx)
 out=`echo ${out} | sed s/\ /_/ | sed -e 's/\(.*\)/\L\1/'`
 echo "Writing to $out."
 xlsx2csv --all "${i}" "${out}"
 echo "Done."
done

echo finished at `date +%Y-%m-%d-%H-%M-%S`
