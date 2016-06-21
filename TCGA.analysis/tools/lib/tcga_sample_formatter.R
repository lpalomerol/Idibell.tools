patient.WITH_TISSUE_FORMAT=15
patient.WITHOUT_TISSUE_FORMAT=12
tcga_sample_format <- function(tcga_code, format = patient.WITH_TISSUE_FORMAT){
  return(
    gsub('\\.', '-', substr(tcga_code, 1, format))
  )
}