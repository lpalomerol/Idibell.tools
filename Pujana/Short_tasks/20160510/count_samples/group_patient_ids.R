#Retrieves the list of patients and groups by tumoric-normal samples

get_patient_id <- function(name){
  return (substr(name, 0,12))
}
get_patient_sample <- function(name){
  return (substr(name, 14,15))
}


patients <- read.table('./data/expression_samples.txt')
colnames(patients) <- c('barcode')
patients$id = get_patient_id(patients$barcode)
patients$code = get_patient_sample(patients$barcode)

table(patients$code)
summary_data <- data.frame(
  patient= samples[!duplicated(samples$id)== TRUE,]$id,
  primary_tumor = NA,
  metastatic = NA,
  normal = NA
)

samples = patients
for(i in 1:nrow(samples)){
  sample = samples[i,]
  if(sample$code == '01'){
    summary_data[summary_data$patient == sample$id,]$primary_tumor = as.character(sample$barcode)
  } else if( sample$code == '06'){
    summary_data[summary_data$patient == sample$id,]$metastatic = as.character(sample$barcode)
  } else {
    summary_data[summary_data$patient == sample$id,]$normal = as.character(sample$barcode)
  }
}    

write.csv(summary_data, './output/expression_by_patient.txt')
