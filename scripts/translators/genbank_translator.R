#!/usr/bin/env Rscript
library("optparse")


option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file name", metavar="character"),
  make_option(c("-o", "--out"), type="character", default="out.txt", 
              help="output file name [default= %default]", metavar="character")
  
); 

description = 'This script receives a tabulated file of genebank accession numbers and attaches genes symbols and names'

opt_parser = OptionParser(option_list=option_list, description=description);
opt = parse_args(opt_parser);

# opt$file = 'c:/Temp/accession_ids_head.txt'
# opt$out = 'c:/Temp/accession_ids_head_out.txt'
source("https://bioconductor.org/biocLite.R")
biocLite("org.Hs.eg.db")
library(org.Hs.eg.db)



accession = read.csv(opt$file, sep="\t")

print("Start retrieving identifiers...")
accessionWithSymbols = select(org.Hs.eg.db,
                              keys= as.character(accession$GB_ACC),
                              columns = c("ENTREZID", "SYMBOL", "GENENAME"),
                              keytype = "ACCNUM")
print("Ok, identifiers retrieved...")
accessionMerged = merge(accession, accessionWithSymbols, by.x='GB_ACC', by.y='ACCNUM')

write.csv(accessionMerged, opt$out )
print("Done!")
