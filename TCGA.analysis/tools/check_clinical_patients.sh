#!/bin/bash

CLINICAL_DATA_FILE=$1

cat $CLINICAL_DATA_FILE | awk '{print $2}' | grep "TCGA"
