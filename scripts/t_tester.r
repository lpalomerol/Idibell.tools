#!/usr/bin/env Rscript
library("optparse")

gene_expression.load <- function(filename, first_column_name=''){
  expression_file = read.csv(filename, sep=' ', as.is=TRUE, header=TRUE)  
  if(first_column_name != ''){
    rownames(expression_file) =   expression_file[,first_column_name]
  }
  expression_file[!colnames(expression_file) %in% c(first_column_name)]
}

gene_expression.0_to_NA <- function(expression){
  expression[expression==0] <- NA
  return(expression)
}

gene_expression.normalize_tcga_names <- function(names){
    gsub('\\.', '-',substr(names, 0,15))
}

gene_expression.filter <- function(expression, ids){
  filtered_expression= expression[,ids]
  rownames(filtered_expression) = rownames(expression)
  filtered_expression
}

make_test <- function(array1, array2){
  test_result = list(
    p.value = NA,
    x.mean =  NA,
    y.mean =  NA,
    x.n = sum(!is.na(array1)),
    y.n = sum(!is.na(array2))
  )
  tryCatch({
    res = t.test(array1, array2)
    test_result = list(
      p.value = res$p.value,
      x.n = sum(!is.na(array1)),
      y.n = sum(!is.na(array2)),
      x.mean = res$estimate[1][[1]],
      y.mean = res$estimate[2][[1]]
    )
  }, error = function(err){
    print(paste("Error doing the t-test", err))
  },finally={
    return(test_result)    
  })
}

option_list = list(
  make_option(c("-e", "--expression-file"), type="character", default=NULL, 
              help="Expression data file", metavar="character"),
  make_option(c("-l", "--left"), type="character", default=NULL, 
              help="Subgroup1 of samples which will be compared", metavar="character"),  
  make_option(c("-r", "--right"), type="character", default=NULL, 
              help="Subgroup2 of samples which will be compared", metavar="character"),  
  make_option(c("-o", "--out"), type="character", default="out.txt", 
              help="output file name [default= %default]", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser)

# opt$'expression-file' = "C:/Users/lpalomero/Documents/TCGA_DATA/prad_data/RNASeqV2/data_expression_genes_merged.txt"
# opt$left = "c:/Temp/2.2.Compare_relapse_groups/splitted_samples/1-ERG.csv"
# opt$right = "../1.input/0-control_ids.csv"
# opt$out= "../3.output/1-ERG.csv"

output_file = opt$out

expression = gene_expression.load(opt$'expression-file', 'gene_id')
expression = gene_expression.0_to_NA(expression)
colnames(expression) = gene_expression.normalize_tcga_names(colnames(expression))

genes = rownames(expression)

left_subgroup = read.csv(opt$left, as.is=TRUE, header = FALSE)
colnames(left_subgroup) = c('Ids')
right_subgroup = read.csv(opt$right, as.is=TRUE, header=FALSE)
colnames(right_subgroup) = c('Ids')

expression_left = gene_expression.filter(expression, left_subgroup$Ids)
expression_right = gene_expression.filter(expression, right_subgroup$Ids)

test_results = data.frame(
  p.value = rep(NA, length(genes)),
  x.mean =  rep(NA, length(genes)),
  y.mean =  rep(NA, length(genes)),
  x.n  = rep(NA, length(genes)),
  y.n  = rep(NA, length(genes))
)

rownames(test_results) <- genes

print(paste("Lets start the checks for ", opt$'expression-file'))
for(i in 1:nrow(expression)){
  if(i %% 10 == 0){
    print(i)
  }
  gene = genes[i]
  tmp_result = make_test(
    expression_left[i,],
    expression_right[i,]
  )
  test_results[gene,]$p.value = tmp_result$p.value
  test_results[gene,]$x.mean = tmp_result$x.mean
  test_results[gene,]$y.mean = tmp_result$y.mean
  test_results[gene,]$x.n = tmp_result$x.n
  test_results[gene,]$y.n = tmp_result$y.n
}

print("Filter non significative results")
significative = test_results[(  (  is.na(test_results$p.value)==FALSE)  & test_results$p.value<0.05),]

print(paste("And save", nrow(significative), "results in ", output_file))
write.table(test_results, paste(output_file, 'all', sep='.'), sep=',', quote=FALSE)
write.table(significative, output_file, sep=',', quote=FALSE)


