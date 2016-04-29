################################################################################
# This scripts generates a Cox PH Model of the relationship of HMMR gene 
#   involved with Breast Cancer (BRCA)
# Luis Palomero - 2016 04 28
################################################################################


#Dependencies
library(survival)

source('../tools/survival_analysis_tools.R')

#Input files

clinical_data <- load_clinical_data('./output/data_clinical_followup_redux.txt')

expression_data <- load_gene_expression_data('./output/data_expression_genes_filtered.txt')

patient_data_fields <- append(BASIC_SURVIVAL_FIELDS, 'HMMR')
brca_patient_data <- build_patient_expression_dataset(clinical_data, expression_data, patient_data_fields)


############################
## COX MODELS GENERATION
############################

#Survival models
death.surv = Surv(brca_patient_data$time.death, brca_patient_data$event.death==1)
recur.surv = Surv(brca_patient_data$time.recur, brca_patient_data$event.recur==1)

#Event  -> Death
#Factor -> HMMR
summary(coxph(death.surv ~ HMMR , data=brca_patient_data))

#Event  -> Recur
#Factor -> HMMR

summary(coxph(recur.surv ~ HMMR , data=brca_patient_data))
