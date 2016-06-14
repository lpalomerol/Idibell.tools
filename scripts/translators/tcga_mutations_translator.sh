#!/usr/bin/env bash

function tcga_mutations_translator.to_polyphen {
  MUTATIONS_FILE=$1
  awk '{print "chr"$5 ":" $6 " " $12"/"$13}' $MUTATIONS_FILE  | grep -v "Start_Position"
}


function tcga_mutations_translator.to_provean {
  MUTATIONS_FILE=$1
  awk '{print $5 "," $6 "," $12","$13}' $MUTATIONS_FILE  | grep -v "Start_Position" | sed 's/-/\./'
}
