#This script receives two survival files (A and B) and generates the three lists: 
# Samples in file A, samples in file B and samples in both files

fileA <- read.csv('./data/data_followup_base.txt', sep=' ')
fileB <- read.csv('./data/data_followup_current.txt', sep='\t')

head(fileA)
head(fileB)

fileA$bcr_patient_barcode <- substring(fileA$barcode, 0, 12)
head(fileA)

common = merge(fileA, fileB, by='bcr_patient_barcode')$bcr_patient_barcode

fileA_only = subset(fileA, !(fileA$bcr_patient_barcode %in% common))
fileA_common = subset(fileA, (fileA$bcr_patient_barcode %in% common))
fileB_only = subset(fileB, !(fileB$bcr_patient_barcode %in% common))
fileB_common =subset(fileB, (fileB$bcr_patient_barcode %in% common))

write.table(fileA_only, './data/data_followup_base_non_common.txt', sep='\t')
write.table(fileA_common, './data/data_followup_base_common.txt', sep='\t')
write.table(fileB_only, './data/data_followup_current_non_common.txt', sep='\t')
write.table(fileB_common, './data/data_followup_current_common.txt', sep='\t')
