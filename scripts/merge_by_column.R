#!/usr/bin/env Rscript
library("optparse")

#This file merges two files by a common column and stores the result in a third file

merge_ignoring <- function(file_a, file_b, by, all){
  file_a[by]=toupper(as.character(file_a[[by]]))
  file_b[by]=toupper(as.character(file_b[[by]]))
  merge(file_a, file_b, by=by, all=all)

}

option_list = list(
  make_option(c('-l', '--left'), type='character', default=NULL,
              help='First file to merge', metavar='character'),
  make_option(c('-r', '--right'), type='character', default=NULL,
              help='Second file to merge', metavar='character'),
  make_option(c('-o', '--output'), type='character', default=NULL,
              help='Output file', metavar='character'),
  make_option(c('-c', '--column'), type='character', default=NULL,
              help='Common column to merge', metavar='character'),
  make_option(c('-i', '--ignore_case'), action='store_true', default=FALSE,
              help="Merge columns ignoring casing")

)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

FILE_A = opt$left # Location of first file to merge
FILE_B = opt$right # Location of second file to merge
COLUMN = opt$column# Column to merge
OUTPUT_FILE = opt$output #Location of result file
IGNORE_CASE= opt$ignore_case # Ignore character casing when merge

if(is.null(COLUMN)){
  stop("COLUMN field is necessary")
} else if(is.null(OUTPUT_FILE)){
  stop("OUTPUT field is necessary")
}


file_a = read.csv(FILE_A, sep=';', as.is=TRUE)
file_b = read.csv(FILE_B, sep=';', as.is=TRUE)

if(IGNORE_CASE){
  merged = merge_ignoring(file_a, file_b, by=COLUMN, all=TRUE )
} else {
  merged = merge(file_a, file_b, by=COLUMN, all = TRUE)
}

write.table(merged, OUTPUT_FILE, sep=";")

print(paste('Ok, file generated at', OUTPUT_FILE))
