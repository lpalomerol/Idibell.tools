###########################################################################
# Compares old followup data with current TCGA follow up data retrieved
# Luis Palomero  - 20160429
###########################################################################


#Load data
old_data <- read.csv('./data/data.followup.base.txt', header = TRUE, sep=' ')
curr_data <- read.csv('./data/data_followup_current.txt', header = TRUE, sep='\t')

head(old_data)
head(curr_data)

#Subsetting current data in old_data format
curr_data_redux <- data.frame(
  curr_data$bcr_patient_barcode,
  curr_data$event.death,
  curr_data$event.recur,
  curr_data$time.death,
  curr_data$time.recur
)

names(curr_data_redux) <- c('barcode', 'event.death', 'event.recur', 'time.death', 'time.recur')

#Renaming old data barcode for making it compatible with new data
old_data$barcode = substr(old_data$barcode, 0, 12)

#Merging the two fields 

merged_data = merge(old_data, curr_data_redux,by='barcode', all=TRUE , suffixes = c('.old', '.cur'))

#Generate comparisons
merged_data$event.death.compared <- merged_data$event.death.old == merged_data$event.death.cur
merged_data$event.recur.compared <- merged_data$event.recur.old == merged_data$event.recur.cur
merged_data$time_death.compared <- merged_data$time.death.old - merged_data$time.death.cur
merged_data$time_recur.compared <- merged_data$time.recur.old - merged_data$time.recur.cur

#Write data
write.table(merged_data, file='./output/compared_data.csv', col.names=TRUE, row.names = FALSE)
