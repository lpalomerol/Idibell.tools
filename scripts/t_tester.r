#!/usr/bin/env Rscript
library("optparse")
library("parallel")

INCLUDE=TRUE
EXCLUDE=FALSE

LENGTH_SAMPLE_ID=15

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
  gsub('\\.', '-',substr(names, 0,LENGTH_SAMPLE_ID))
}

gene_expression.filter <- function(expression, ids, action = INCLUDE){
  if(action==INCLUDE){
    expression[,colnames(expression) %in% ids]
  } else {
    expression[,!(colnames(expression) %in% ids)]
  }
}


make_test <- function(array1, array2, n){
  test_result = list(
    statistic = NA,
    p.value = NA,
    fdr = NA,
    x.mean =  NA,
    y.mean =  NA,
    x.n = sum(!is.na(array1)),
    y.n = sum(!is.na(array2))
  )
  tryCatch({
    res = t.test(array1, array2)
    test_result = list(
      statistic = res$statistic,
      p.value = res$p.value,
      fdr = p.adjust(res$p.value, 'fdr', n),
      x.mean = res$estimate[1][[1]],
      y.mean = res$estimate[2][[1]],
      x.n = sum(!is.na(array1)),
      y.n = sum(!is.na(array2))
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
 
# opt$'expression-file' = "C:/Users/lpalomero/Documents/TCGA_DATA/prad_data/RNASeqV2/data_expression_genes_merged.txt"
# opt$left = "c:/Temp/2.2.Compare_relapse_groups/splitted_samples/1-ERG.csv"
# opt$right= "c:/Users/lpalomero/Dropbox/1.Grupos/2.Aytes/Projects/1.p53_castration/2.create_basic_tcga_signatures/1.input/0-control_ids.csv"
# opt$'gene-ids-column' = 'gene_id'
# opt$out= "c:/Users/lpalomero/Dropbox/1.Grupos/2.Aytes/Projects/1.p53_castration/2.create_basic_tcga_signatures/3.output/t_tests.csv"

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


items = 1:nrow(expression)
print(paste("Lets start the checks for ", opt$'expression-file'))
ptm <- proc.time()

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

test_results = parSapply(cl, items, function(i, genes, expression_left, expression_right, make_test){
  gene = genes[i]
  tmp_result = make_test(
    as.numeric(expression_left[i,]),
    as.numeric(expression_right[i,]),
    n=length(genes)
  )
  return(tmp_result)
}, genes=genes, expression_left=expression_left, expression_right=expression_right, make_test=make_test)

stopCluster(cl)

proc.time() - ptm
print("Filter non significative results")

colnames(test_results) <- genes
test_results = t(test_results)

significative = test_results[(  (  is.na(test_results[,'p.value'])==FALSE)  & test_results[,'p.value']<0.05),]

print(paste("And save", nrow(significative), "results in ", output_file))

write.table(test_results, paste(output_file, 'all', sep='.'), sep=',', quote=FALSE)
write.table(significative, output_file, sep=',', quote=FALSE)

