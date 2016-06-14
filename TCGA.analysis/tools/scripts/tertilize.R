#Returns the list of all samples splitted by tertiles (low-mid-high)
args = commandArgs(trailingOnly = TRUE)

EXPRESSION_PATH_ID=1
GENE_ID=2
OUTPUT_FOLDER_ID=3

FIRST_TERTILE=1
SECOND_TERTILE=2
THIRD_TERTILE=3

expression_path=args[EXPRESSION_PATH_ID]
gene = args[GENE_ID]
output_folder=args[OUTPUT_FOLDER_ID]
expression = read.table(expression_path, as.is=TRUE)
expression_filtered = as.numeric(expression[gene,])
expression_tertiles = quantile(expression_filtered,c(1/3, 2/3, 1))

expression_tertiles_id = ifelse(expression_filtered < expression_tertiles[FIRST_TERTILE], FIRST_TERTILE, 
                                ifelse(expression_filtered < expression_tertiles[SECOND_TERTILE], SECOND_TERTILE, THIRD_TERTILE))

expression_low = colnames(expression)[expression_tertiles_id==FIRST_TERTILE]
expression_mid = colnames(expression)[expression_tertiles_id==SECOND_TERTILE]
expression_high = colnames(expression)[expression_tertiles_id==THIRD_TERTILE]

build_ouput_path <- function(output_folder, gene, tertile_name){

  tertile_string =  ifelse(tertile_name == FIRST_TERTILE, 'low', 
                        ifelse(tertile_name == SECOND_TERTILE, 'mid', 'high'))

  file_name = paste(gene, '_', tertile_string, '_tertile.txt', sep='')
  return(paste(output_folder, file_name, sep='/'))
}

write_expr <- function(dataset, destination){
  write.table(
    dataset,
    destination,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE
  )
  
}
write_expr(expression_low, build_ouput_path(output_folder, gene, FIRST_TERTILE))
write_expr(expression_mid, build_ouput_path(output_folder, gene, SECOND_TERTILE))
write_expr(expression_high, build_ouput_path(output_folder, gene, THIRD_TERTILE))
