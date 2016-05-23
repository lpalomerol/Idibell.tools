#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
#The objective of this file is remove the invalid genes at one gene list.
# INPUT PARAMS:
#   - list of "all" genes
#   - list of "invalid" genes
#   - output file
# IMPORTANT ABOUT DATA FORMAT: Both input files should have a first column called "symbol"
# Usage sample : remove_invalid_genes.R all_genes.txt invalid_genes.txt output.txt

ALL_GENES_FILE = args[1]
INVALID_GENES_FILE = args[2]
OUTPUT_FILE = args[3]

all_genes = read.csv(ALL_GENES_FILE)
invalid_genes = read.csv(INVALID_GENES_FILE)

filtered_genes = (all_genes[!(all_genes$symbol %in% invalid_genes$symbol),])
write.table(filtered_genes, OUTPUT_FILE, col.names = FALSE, 
            row.names = FALSE, quote = FALSE)
print(paste("OK, output file generated at", OUTPUT_FILE))