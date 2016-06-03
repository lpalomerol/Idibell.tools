#!/usr/bin/env Rscript
library("optparse")

option_list = list(
  make_option(c('-i', '--input'), type='character', default=NULL,
              help='Input file', metavar='character'),
  make_option(c('-o', '--output'), type='character', default=NULL,
              help='Output file', metavar='character'),
  make_option(c('-m', '--method'), type='character', default='log2',
              help='Normalization method [default= %default]', metavar='character'),
  make_option(c('-s', '--separator'), type='character', default='\t',
              help='File separator (for input and output) [default= %default]', metavar='character')

)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

method= opt$method
input_path = opt$input
separator = opt$separator
output_path = opt$output

strategy = NA

dataframe = read.csv(input_path, sep=separator, as.is=TRUE)

if(method == 'log') {
  strategy <- function(line){
    return(log(line))
  }
} else if (method == 'log2') {
  strategy <- function(line){
    return(log2(line))
  }
} else if (method == 'log10') {
  strategy <- function(line){
    return(log10(line))
  }
}

normalize <- function(line){
  if(is.numeric(line) == TRUE){
    return(strategy(line))
  } else{
    return(line)
  }
}

normalized = lapply(dataframe, normalize)
write.table(normalized, output_path, sep=separator, row.names = FALSE)