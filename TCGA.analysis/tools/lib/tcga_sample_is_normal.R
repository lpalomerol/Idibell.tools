TCGA_SAMPLE_TYPE_START_ID=14
TCGA_SAMPLE_TYPE_END_ID=15
TcgaSampleIsNormal <- function(tcga_code){
  return(substr(tcga_code, TCGA_SAMPLE_TYPE_START_ID,TCGA_SAMPLE_TYPE_END_ID) %in% c('10', '11','12','13', '14'))
}

