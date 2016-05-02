#Constants
REDUCED_CLINICAL_DATA_BASIC_FIELDS <- c(
  'bcr_patient_uuid',
  'bcr_patient_barcode',
  'histological_type',
  'gender',
  'ajcc_tumor_pathologic_pt',
  'ajcc_nodes_pathologic_pn',
  'ajcc_metastasis_pathologic_pm',
  'ajcc_pathologic_tumor_stage',
  'vital_status',
  'last_contact_days_to',
  'death_days_to',
  'new_tumor_event_dx_days_to'
)

#Functions
load_clinical_data = function(file){
  raw = readLines(file)
  #Remove unwanted headers
  raw = raw[-2:-3]
  read.csv(textConnection(raw), header = TRUE, stringsAsFactors = FALSE, sep='\t')
}

clean_clinical_data = function(clinical_data){
  return(subset(clinical_data, 
                !(clinical_data$vital_status == 'Alive' 
                  & clinical_data$last_contact_days_to == '[Not Available]')))
}

save_clinical_data <- function(data, path ){
  data = replace(data, is.na(data), '[Not Available]')
  write.table(data, file=path, sep='\t', row.names = FALSE)
}

attach_followup_data <- function(clinical_data, followup_data){
  merge(clinical_data, followup_data, by = "bcr_patient_barcode", all=T, suffixes = c('', '.y'))
}

get_reduced_clinical_data <- function(clinical_data, fields=REDUCED_CLINICAL_DATA_BASIC_FIELDS){
  clinical_data[ , (names(clinical_data) %in% fields)]
}


attach_events_data <- function(clinical_data){
  clinical_data['event.death'] = check_is_dead(clinical_data)
  clinical_data[,'event.recur'] = check_is_recur(clinical_data)
  clinical_data[,'time.death'] = get_death_time(clinical_data)
  clinical_data[,'time.recur'] = get_recur_time(clinical_data)
  clinical_data
}


check_is_dead <- function(data){
  return (apply(data['vital_status'], 1, FUN = function(value){ 
    if (value == 'Dead'){ 
      return (1) 
    } else { 
      return (0)
    }
  }))
}

check_is_recur <- function(data){
  chec_recur <- function(line) {
    if (!is.na(line['new_tumor_event_dx_days_to'])) {
      return (1)
    } else {
      return (0)
    }
  }
  return  (apply(data, 1, FUN = chec_recur ))
}

get_line_death_time <- function(line){
    if(line['event.death'] == 1) {
      days = line['death_days_to']
    } else {
      days = line['last_contact_days_to']
    }
    return (as.numeric(days))
}

get_death_time <- function(data) {
    return (apply(data[,c('event.death', 'death_days_to', 'last_contact_days_to')],
                1, 
                FUN =function(line){floor(get_line_death_time(line)/30)}))
}

get_recur_time <- function(data){
  find_data <- function(line){
    if(line['event.recur'] == 1) {
      days = (line['new_tumor_event_dx_days_to'])
    } else {
      days = ifelse( (line['last_contact_days_to'] != '[Not Available]') , line['last_contact_days_to'],line['death_days_to'] )
    }
    return ( floor(as.numeric(days)/30))
  }
  return (apply(data, 1, FUN = find_data))
}

