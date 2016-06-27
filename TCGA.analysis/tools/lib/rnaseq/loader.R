rnaseqExpressionLoader <- function(rnaseqExpressionPath, sep=';'){
  rnaseqExpression = read.csv(rnaseqExpressionPath, sep=';')
  rownames(rnaseqExpression) =  rnaseqExpression[,'gene_id']
  rnaseqExpression = rnaseqExpression[!names(rnaseqExpression) %in% c('gene_id')]  
  rnaseqExpression[rnaseqExpression == 0] <- NA
  return(rnaseqExpression)
}