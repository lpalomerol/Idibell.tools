############################################################
# Merges clinical data files and generates new output files
# Author: Luis Palomero, 2016/04/25
############################################################

setwd("C:/Users/lpalomero/Desktop/coad_data/Clinical_survival")

loadFile = function(file){
  raw = readLines(file)
  raw = raw[-2:-3]
  read.csv(textConnection(raw), header = TRUE, stringsAsFactors = FALSE, sep='\t')
}

saveFile = function(data, output_name){
  data = replace(data, is.na(data), '[Not Available]')
  write.table(data, file=output_name, sep='\t', row.names = FALSE)
}

clinical_data_name = 'nationwidechildrens.org_clinical_patient_coad.txt'
follow_up_name = 'nationwidechildrens.org_clinical_follow_up_v1.0_nte_coad.txt'

output_name = 'nationwidechildrens.org_clinical_patient_coad_follow_up.txt'
output_filtered_name = 'nationwidechildrens.org_clinical_patient_coad_follow_up_filtered_.txt'

clinical_data = loadFile(clinical_data_name)
follow_up = loadFile(follow_up_name)

#Merge files
clinical_data_merged = merge(clinical_data, follow_up, by = "bcr_patient_barcode", all=T, suffixes = )
saveFile(clinical_data_merged, output_name)

#Create a file with filtered data
keeps <- c(
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

clinical_data_merged_filtered = clinical_data_merged[ , (names(clinical_data_merged) %in% keeps)]


#Add extra fields
is_dead <- function(data){
  return (apply(data['vital_status'], 1, FUN = function(value){ if (value == 'Dead'){ return (1) } else { return (0)}}))
}

is_recur <- function(data){
  chec_recur <- function(line) {
    if (!is.na(line['new_tumor_event_dx_days_to'])) {
      return (1)
    } else {
      return (0)
    }
  }
  return  (apply(data, 1, FUN = chec_recur ))
}

get_death_time <- function(data) {
 find_data <- function(line){
   if(line['event.death'] == 1) {
     days = (line['death_days_to'])
   } else {
     days = (line['last_contact_days_to'])
   }
   return ( floor(as.numeric(days)/30))
 }
 return (apply(data, 1, FUN = find_data))
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

clinical_data_merged_filtered['event.death'] = is_dead(clinical_data_merged_filtered)
clinical_data_merged_filtered[,'event.recur'] = is_recur(clinical_data_merged_filtered)
clinical_data_merged_filtered[,'time.death'] = get_death_time(clinical_data_merged_filtered)
clinical_data_merged_filtered[,'time.recur'] = get_recur_time(clinical_data_merged_filtered)

saveFile(clinical_data_merged_filtered, output_filtered_name)

