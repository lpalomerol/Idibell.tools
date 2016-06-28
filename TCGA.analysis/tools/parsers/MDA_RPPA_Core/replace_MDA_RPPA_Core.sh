#!/usr/bin/env bash


EXPRESSION_FILES_DIR=${1-expression_files} # Directory where expression files are stored
CATALOG=${2-./expression_catalog/catalog.txt} #File with gene exprssion catalog

TMP_FILE=tmp_replaces.sh

echo "Start preparing the file"
cut $CATALOG -f5,10 | grep -v "Sample Name" | sed 's/..$//' | sort | uniq | awk '{print "echo "$1"; sed -i \"s/"$1"/"$2"/\" ./'$EXPRESSION_FILES_DIR'/*"$1"*"}' > $TMP_FILE
exit
echo "Now lets start the replacements.."
sh $TMP_FILE
echo "And finally... clean"
rm $TMP_FILE
