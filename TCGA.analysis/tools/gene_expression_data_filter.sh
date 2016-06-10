#!/bin/bash

#The objective of this script is remove the unnecessary gene values from the
#  gene expression data matrix in order to simplify R scripts.

DATA_FILE=$1
GENE_LIST=$2 #Gene list can be one item or multiple, for example '"ACTA"' or '"ACTA\|VIM"'
OUTPUT=$3
HEADER=${4-bcr_patient_barcode}

GENE_LIST="$HEADER\|$GENE_LIST"

echo "Filtering $GENE_LIST at $DATA_FILE"
grep -w $GENE_LIST $DATA_FILE > $OUTPUT
echo "Done!, check $OUTPUT"
