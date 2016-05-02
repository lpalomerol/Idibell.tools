################################################################################
# This scripts generates a Cox PH Model of the relationship of LOXL2 gene 
#   involved with Breast Cancer (BRCA)
# Luis Palomero - 2016 04 29
################################################################################


#Dependencies
library(survival)

source('../tools/survival_analysis_tools.R')

#Input files

clinical_data <- read.csv('./data/data.followup.base.txt', head=T, sep=' ')

clinical_data$bcr_patient_barcode = substr(clinical_data$barcode,0,12)

expression_data <- load_gene_expression_data('./data/data_expression_genes_filtered_loxl2.txt')

patient_data_fields <- append(BASIC_SURVIVAL_FIELDS, 'LOXL2')
brca_patient_data <- build_patient_expression_dataset(clinical_data, expression_data, patient_data_fields)

head(brca_patient_data)
############################
## COX MODELS GENERATION
############################

#Survival models
death.surv = Surv(brca_patient_data$time.death, brca_patient_data$event.death==1)
recur.surv = Surv(brca_patient_data$time.recur, brca_patient_data$event.recur==1)

#Event  -> Death
#Factor -> HMMR
summary(coxph(death.surv ~ LOXL2 , data=brca_patient_data))

#Event  -> Recur
#Factor -> HMMR

summary(coxph(recur.surv ~ LOXL2 , data=brca_patient_data))
