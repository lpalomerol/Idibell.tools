#!/bin/bash

if [ "$1" == "-h" ]; then
  echo "This script obtains the relation between the patient codes of gene expression file with TCGA patient barcodes"
  echo "  For example, metadata file is placed at ./METADATA/BI__HT_HG-U133A/broad.mit.edu_OV.HT_HG-U133A.sdrf.txt"
  echo ""
  echo "Usage: `basename $0` /path/to/metadata/file"
  echo ""
  exit 0
fi

METADATA_FILE=$1

echo "tcga_patient_barcode affy_barcode"
cut $METADATA_FILE -f2,13 | grep -v "Comment"
