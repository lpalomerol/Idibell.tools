#Removes normal tissue samples from expression matrix
args = commandArgs(trailingOnly = TRUE)


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

is_normal_tissue <- function(tissue){
  (as.numeric(strsplit(tissue, '-')[[1]][4])/10)>1
}

EXPRESSION_MATRIX_PATH_ID = 1
EXPRESSION_MATRIX_CANCER_OUTPUT_PATH_ID = 2
EXPRESSION_MATRIX_NORMAL_OUTPUT_PATH_ID = 3

expression = gene_expression.load(args[EXPRESSION_MATRIX_PATH_ID], 'bcr_patient_barcode')
expression = gene_expression.0_to_NA(expression)
samples = gene_expression.normalize_tcga_names(colnames(expression))

normal_tissues = sapply(samples, is_normal_tissue, USE.NAMES = FALSE)

cancer_expressions = expression[,!normal_tissues]
normal_expressions = expression[,normal_tissues]
expression_matrix_output = args[EXPRESSION_MATRIX_PATH_ID]

write.table(cancer_expressions, args[EXPRESSION_MATRIX_CANCER_OUTPUT_PATH_ID])
write.table(normal_expressions, args[EXPRESSION_MATRIX_NORMAL_OUTPUT_PATH_ID])