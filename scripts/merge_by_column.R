#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#This file merges two files by a common column and stores the result in a third file

FILE_A = args[1] # Location of first file to merge
FILE_B = args[2] # Location of second file to merge
COLUMN = args[3] # Column to merge 
OUTPUT_FILE = args[4] #Location of result file

file_a = read.csv(FILE_A, sep=';')
file_b = read.csv(FILE_B, sep=';')

merged = merge(file_a, file_b, by=COLUMN, all = TRUE)
write.table(merged,
            OUTPUT_FILE, row.names = FALSE, sep=';')

print(paste('Ok, file generated at', OUTPUT_FILE))