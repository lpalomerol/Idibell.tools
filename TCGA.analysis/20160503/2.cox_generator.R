#Loads the different files data and generates the 3 cox models 
#  (common in "base", common in "current" and uncommon in "current")

getwd()

library(survival)
source('../tools/survival_analysis_tools.R')


#Generate all files
expression_filename <- './data/data_expression_genes_filtered_loxl2.txt'

common_samples_base  <- get_clinical_and_expression_data('./data/data_followup_base_common.txt', 
                                                         expression_filename,
                                                         'LOXL2')

common_samples_curr <- get_clinical_and_expression_data('./data/data_followup_current_common.txt',
                                                        expression_filename,
                                                        'LOXL2')

uncommon_samples_curr <- get_clinical_and_expression_data('./data/data_followup_current_non_common.txt',
                                                          expression_filename,
                                                          'LOXL2')

head(common_samples_base)
head(common_samples_curr)
head(uncommon_samples_curr)

common_base_cox <- build_surv_models(common_samples_base)
common_curr_cox <- build_surv_models(common_samples_curr)
uncommon_curr_cox <- build_surv_models(uncommon_samples_curr)

summary(coxph(common_base_cox$death ~ LOXL2 , data=common_samples_base))
summary(coxph(common_curr_cox$death ~ LOXL2 , data=common_samples_curr))
summary(coxph(uncommon_curr_cox$death ~ LOXL2 , data=uncommon_samples_curr))

summary(coxph(common_base_cox$recur ~ LOXL2 , data=common_samples_base))
summary(coxph(common_curr_cox$recur ~ LOXL2 , data=common_samples_curr))
summary(coxph(uncommon_curr_cox$recur ~ LOXL2 , data=uncommon_samples_curr))

(common_base_cox$recur)
