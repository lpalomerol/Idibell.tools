#Retrieves the list of patients and groups by tumoric-normal samples
getwd()
patient_ids_file  <- './data/data_expression_equivalencies.txt' 
output_file <-  './output/expression_by_patient.txt'
get_patient_id <- function(name){
    return (substr(as.character(name), 0,12))
}
get_patient_sample <- function(name){
  return (substr(as.character(name), 14,15))
}


patients <- read.table(patient_ids_file)
head(patients)
colnames(patients) <- c('barcode', 'affy_barcode')
patients$id = get_patient_id(patients$barcode)
patients$code = get_patient_sample(patients$barcode)

table(patients$code)

samples = patients
summary_data <- data.frame(
  patient= samples[!duplicated(samples$id)== TRUE,]$id,
  primary = NA,
  recurrent = NA,
  mestastatic = NA,
  normal = NA
)


for(i in 1:nrow(samples)){
  sample = samples[i,]
  chr_barcode = as.character(sample$barcode)
  if(sample$code == '01'){
    summary_data[summary_data$patient == sample$id,]$primary = chr_barcode
  } else if( sample$code == '02'){
    summary_data[summary_data$patient == sample$id,]$recurrent = chr_barcode 
  } else if( sample$code == '06'){
    summary_data[summary_data$patient == sample$id,]$metastatic = chr_barcode
  } else if(sample$code == '11') {
    summary_data[summary_data$patient == sample$id,]$normal = chr_barcode
  }
}    

write.csv(summary_data, output_file)
