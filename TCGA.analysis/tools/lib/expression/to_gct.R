expression.to_cgt <- function(expression, path){
  file = file(path, "w")
  writeLines("#1.2", file)
  expression_columns = ncol(expression) - 1
  writeLines(paste(nrow(expression), expression_columns, sep='\t'), file)
  names = colnames(expression)
  names(expression)[names(expression) == 'bcr_patient_barcode'] <-  'NAME'

  expression = expression[,c(1, 1, 2:ncol(expression))]
  names(expression)[2] = 'Description'
  expression$Description='na'
  write.table(expression, file, sep='\t', quote=FALSE)
  close(file)
}