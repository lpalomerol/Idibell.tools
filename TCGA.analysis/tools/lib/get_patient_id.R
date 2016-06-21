get_patient_id <- function(tcga_code){
  return(
    gsub('\\.', '-', substr(tcga_code, 1,15))
  )
}