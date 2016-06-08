#!/usr/bin/env bash

EXPRESSION_FILES_DIR='expression_files' # Directory where expression files are stored
SED_TASKS='seds.txt' #Output file, it will tore the sed commands
CATALOG="./expression_catalog/catalog.txt" #File with gene exprssion catalog
REPLACING_SEDS='replacing_seds.txt'

echo "The idea is generate a catalog of replacing fies."

echo "" > $SED_TASKS
awk '{print "sed -i \"s/normalized_count/"FILENAME"/\" "FILENAME}'  ./$EXPRESSION_FILES_DIR/*normalized_results |  sed "s/\.\/$EXPRESSION_FILES_DIR\///" | uniq >> $SED_TASKS

echo "The first catalog is almost generated, now the idea is replace the name of the file by TCGA id"

echo "Below sentence generates a list of sentences which replaces the file name by TCGA id at $SED_TASKS" 
grep "rsem\.genes\.normalized_results" $CATALOG  | cut -d$'\t' -f2,22 | awk '{print "sed -i \"s/normalized_count\\\/"$2"/normalized_count\\\/"$1"/\" '$SED_TASKS';"}' > $REPLACING_SEDS

sh $REPLACING_SEDS

echo "Done! Now the file $SED_TASKS includes the list of bash sentences which replace \"normalized_results\" column by TCGA id..."
echo "Next task is execute it... enjoy!"

rm $REPLACING_SEDS # Cleaning tmp files...
