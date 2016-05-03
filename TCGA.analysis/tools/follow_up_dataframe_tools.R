#CONSTANTS

v1_5 = '1.5'
v2_1 = '2.1'
v4_0 = '4.0'

load_follow_up_data <- function(file, version='', new_time_event_file = ''){
  if(version == ''){
    stop('Undefined version')
  } else if (version == v1_5 || version == v2_1){
    followup_file = build_followup_file(file)
  } else if (version == v4_0){
    followup_file = build_followup_file_v4_0(file, new_time_event_file)
  } else{
    stop('Invalid version')
  }
  normalize_followup_dataframe(followup_file)
}

build_followup_file <- function(filename){
  raw = readLines(file)
  #Remove unwanted headers
  raw = raw[-2:-3]
  read.csv(textConnection(raw), header = TRUE, stringsAsFactors = FALSE, sep='\t')
}

build_followup_file_v4_0 <- function(followup_file, nte_file){
  merge(
    build_followup_file(followup_file),
    build_followup_file(nte_file),
    col='bcr_patient_barcode',
    all= TRUE
  )
}

normalize_followup_dataframe <- function(dataframe){
  keeps = c('bcr_patient_uuid', 'bcr_patient_barcode', 'new_tumor_event_dx_days_to', 'vital_status', 'last_contact_days_to')
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

