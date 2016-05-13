return ();

library(plyr)

expression_file <- "./data/data_expression_genes_merged.txt"
equivalencies_file  <- "./data/data_expression_equivalencies.txt"
output_file <- "./output/data_expression_genes_merged.txt"

expression <- read.csv(expression_file, sep=" ")
equivalencies <- read.csv(equivalencies_file, sep="")

equivalencies$affy_barcode_norm = 
  apply(equivalencies['affy_barcode'], 2, function(row){
    gsub('-', '.', gsub('.CEL', '',  row))
  }
)

setdiff(
  as.character(colnames(expression)),
  equivalencies$affy_barcode_norm
)

expression_norm = expression
for(i in 1:nrow(equivalencies)){
  equiv = equivalencies[i,]
  affy_code = as.character(equiv$affy_barcode_norm)
  tcga_code = as.character(equiv$tcga_patient_barcode)
  names(expression_norm)[names(expression_norm)==affy_code] <- tcga_code
}

write.csv(expression_norm, file=output_file, sep=' ')
