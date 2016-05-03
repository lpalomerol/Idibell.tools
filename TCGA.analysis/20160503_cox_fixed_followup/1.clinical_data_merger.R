#1 Load followup data files
#Merge followup data
#Create cox models for this new followup data

source('../tools/follow_up_dataframe_tools.R')

dataframes <- list(
  load_follow_up_data('./data/follow_up_v1.5.txt', ver=v1_5),
  load_follow_up_data('./data/follow_up_v2.1.txt', ver=v2_1),
  load_follow_up_data('./data/follow_up_v4.0.txt', './data/follow_up_v4.0_nte.txt', ver=v4_0)
)

follow_ups <- merge_follow_ups(dataframes)
follow_ups <- clean_follow_up_dataframe_data(follow_ups)
