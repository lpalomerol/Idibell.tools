############################################################
# Merges clinical data files and generates new output files
# Author: Luis Palomero, 2016/04/25
############################################################


#External functions
source('tools/follow_up_dataframe_tools.R')
source('tools/clinical_data_dataframe_tools.R')

#The code
dataframes <- list(
  load_follow_up_data('./20160428/data/data_follow_up_v1.5_brca.txt'),
  load_follow_up_data('./20160428/data/data_follow_up_v2.1_brca.txt'),
  load_follow_up_data('./20160428/data/data_follow_up_v4.0_nte_brca.txt')
)

follow_ups <- merge_follow_ups(dataframes)
follow_ups <- clean_follow_up_dataframe_data(follow_ups)

clinical_data <- load_clinical_data('./20160428/data/data_clinical_raw.txt')

clinical_data <- clean_clinical_data(clinical_data)

clinical_data_followup = attach_followup_data(clinical_data, follow_ups)

save_clinical_data(clinical_data_followup, './20160428/output/data_clinical_followup.txt')

clinical_data_redux = get_reduced_clinical_data((clinical_data_followup))

clinical_data_redux_events = attach_events_data(clinical_data_redux)

save_clinical_data(clinical_data_redux_events, './20160428/output/data_clinical_followup_redux.txt')
