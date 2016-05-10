#!/bin/bash

if [ "$1" == "-h" ]; then
  echo "This scripts lists the patients at expression_genes directory"
  echo ""
  echo "Usage: `basename $0` /path/to/raw/expression/genes -l"
  echo ""
  echo "Optional parameters:"
  echo "    -l: Returns long ids of patients, by default is shortened format (TCGA-TSS-PATIENT_ID)"
  exit 0
fi

DIR_GENE_EXPRESSION=$1

SHORT_PARAMETER=$2

echo "Retrieving sample ids from Gene expressiond directory [$1]"

if [ "$2" == "-l" ]; then
  echo "Retrieving LONG patient ids"
  head  $DIR_GENE_EXPRESSION/*.txt -n1 | awk '{print $3}' | cat | grep "T"
else
  echo "Retrieving SHORT patient ids"
  head  $DIR_GENE_EXPRESSION/*.txt -n1 | awk '{print $3}' | cut -d'-' -f1,2,3 | cat | grep "T"
fi
