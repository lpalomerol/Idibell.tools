#!/usr/bin/env Rscript
library("optparse")

INCLUDE=TRUE
EXCLUDE=FALSE

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
  gsub('\\.', '-',substr(names, 0,16))
}

gene_expression.filter <- function(expression, ids, action = INCLUDE){
  if(action==INCLUDE){
    expression[,colnames(expression) %in% ids]
  } else {
    expression[,!(colnames(expression) %in% ids)]
  }
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
  make_option(c('-c', '--gene-ids-column'), type='character', default='',
              help="Name of the column which will be the dataset column" ),
  make_option(c("-o", "--out"), type="character", default="out.txt", 
              help="output file name [default= %default]", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser)

# opt$'expression-file' = "c:/Users/lpalomero/Documents/TCGA_DATA/brca_data/data_expression_genes_merged_only_cancer.txt"
# opt$left = "c:/Users/lpalomero/Dropbox/1.Grupos/8.Pujana/Proyectos/2.breast_cancer_chemoresistance/9.T-test_denovo_enriched/1.input/3.enriched_deNovoDeletereous_TCGA-tumors.txt"
# # opt$right = NULL
# opt$'gene-ids-column' = ''
# opt$out= "c:/Users/lpalomero/Dropbox/1.Grupos/8.Pujana/Proyectos/2.breast_cancer_chemoresistance/9.T-test_denovo_enriched/3.output/t_tests.csv"

output_file = opt$out

expression = gene_expression.load(opt$'expression-file', opt$'gene-ids-column')
expression = gene_expression.0_to_NA(expression)
colnames(expression) = gene_expression.normalize_tcga_names(colnames(expression))

genes = rownames(expression)

left_subgroup = read.csv(opt$left, as.is=TRUE, header = FALSE)
colnames(left_subgroup) = c('Ids')
expression_left = gene_expression.filter(expression, left_subgroup$Ids)

if(is.null(opt$right) == TRUE){
  expression_right = gene_expression.filter(expression, left_subgroup$Ids, EXCLUDE)
} else {
  right_subgroup = read.csv(opt$right, as.is=TRUE, header=FALSE)
  colnames(right_subgroup) = c('Ids')
  expression_right = gene_expression.filter(expression, right_subgroup$Ids)  
}

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
    as.numeric(expression_left[i,]),
    as.numeric(expression_right[i,])
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


