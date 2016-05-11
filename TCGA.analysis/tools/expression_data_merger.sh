#!/bin/bash

sed -i 's/Hybridization/bcr_patient_barcode/' $@
sed -i 's/REF//g' $@

touch prev
echo "Generate PREV file from $1..."


i=0;
j=$i
cut -f1 $1 > prev$i

for FILE in $@; do
  j=$(($i+1))
  echo  "joining...$FILE to prev$i"
  join -a1 -1 1 -2 1 prev$i $FILE > prev$j
  i=$(($i+1))

done

mv prev$i data_expression_genes_merged.txt
rm prev*
#head prev
