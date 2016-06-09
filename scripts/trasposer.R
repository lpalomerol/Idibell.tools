#!/usr/bin/env Rscript
library("optparse")

option_list = list(
  make_option(c('-i', '--input'), type='character', default=NULL,
              help='Input file', metavar='character'),
  make_option(c('-o', '--output'), type='character', default=NULL,
              help='Output file', metavar='character'),
  make_option(c('-s', '--separator'), type='character', default='\t',
              help='File separator (for input and output) [default= %default]', metavar='character')  
)



opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

# print(opt)

input_path = opt$input
separator = opt$separator
output_path = opt$output
# 
# input_path = "C:/Temp/8.4.4.Chiara_ahr_areg/2.pearsons/expU133A.norm.txt"
# separator = "\t"
# output_path =  "C:/Temp/8.4.4.Chiara_ahr_areg/2.pearsons/expU133A.norm_t.txt"

strategy = NA

dataframe = read.csv(input_path, sep=separator, as.is=TRUE)
trasposed = as.data.frame(t(dataframe))

write.table(trasposed, output_path, quote = FALSE, col.names = FALSE, sep=separator)

