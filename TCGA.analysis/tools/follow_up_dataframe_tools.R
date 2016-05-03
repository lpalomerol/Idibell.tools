load_follow_up_data <- function(file){
  raw = readLines(file)
  #Remove unwanted headers
  raw = raw[-2:-3]
  dataframe = read.csv(textConnection(raw), header = TRUE, stringsAsFactors = FALSE, sep='\t')
  keeps = c('bcr_patient_uuid', 'bcr_patient_barcode', 'new_tumor_event_dx_days_to')
  dataframe[,(names(dataframe) %in% keeps)]
}

merge_follow_ups <- function(dataframes) {
  do.call(rbind,dataframes)
}

clean_follow_up_dataframe_data <- function(dataframe){
  dataframe$new_tumor_event_dx_days_to = as.numeric(as.character(dataframe$new_tumor_event_dx_days_to))
  dataframe = dataframe[!is.na(dataframe$new_tumor_event_dx_days_to),]
  dataframe = with(dataframe, dataframe[order(bcr_patient_barcode, new_tumor_event_dx_days_to, bcr_patient_uuid),])
  dataframe[!duplicated(dataframe$bcr_patient_barcode),]
}