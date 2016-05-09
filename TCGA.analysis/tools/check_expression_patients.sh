#!/bin/bash

DIR_GENE_EXPRESSION=$1

echo $DIR_GENE_EXPRESSION

head  $DIR_GENE_EXPRESSION/*.txt -n1 | awk '{print $3}' | cut -d'-' -f1,2,3 | cat | grep "T"
