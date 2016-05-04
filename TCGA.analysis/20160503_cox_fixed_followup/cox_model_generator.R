#1- Load followup data files
#2- Merge expression data
#3- Create cox models for this new followup data
getwd()
library(survival)
source('../tools/follow_up_dataframe_tools.R')
source('../tools/clinical_data_dataframe_tools.R')
source('../tools/survival_analysis_tools.R')



f1 <- load_follow_up_data('./data/follow_up_v1.5.txt', version = v1_5)
f2 <- load_follow_up_data('./data/follow_up_v2.1.txt', version = v2_1)
f4 <- load_follow_up_data('./data/follow_up_v4.0.txt',  './data/follow_up_v4.0_nte.txt', version = v4_0)


head(f1)
head(f2)
head(f4)
ftot = intersect_follow_ups(f4, 
                            intersect_follow_ups(f2,f1))


ftot = add_event_data(ftot)

ftot

expression_data <- load_gene_expression_data('./data/data_expression_genes_filtered_loxl2.txt')

summary.followup(ftot)

colnames(expression_data)
patient_data_fields <- append(BASIC_SURVIVAL_FIELDS, 'LOXL2')
data_with_expression = merge(ftot, expression_data, by.y = 'bcr_patient_barcode', by.x='barcode' )
data_with_expression$LOXL2 = as.numeric(as.character(data_with_expression$LOXL2))
data_with_expression$LOXL2_g = ifelse(data_with_expression$LOXL2 > -0.062124, 3,
                                      ifelse(data_with_expression$LOXL2 > -0.788, 2,
                                             ifelse(data_with_expression$LOXL2 > -1.357312, 1,0)))
summary.followup(data_with_expression)


surv_model <- build_surv_models(data_with_expression)

(surv_model$death)
(surv_model$recur)
summary(coxph(surv_model$death ~ LOXL2 , data=data_with_expression))

summary(coxph(surv_model$recur ~ LOXL2 , data=data_with_expression))

summary(coxph(surv_model$death ~ LOXL2_g , data=data_with_expression))

summary(coxph(surv_model$recur ~ LOXL2_g , data=data_with_expression))

kmfit1 = survfit(surv_model$death ~ LOXL2_g, data=data_with_expression)
kmfit2 = survfit(surv_model$recur ~ LOXL2_g, data=data_with_expression)

summary(kmfit1)
summary(kmfit2)
draw_survplot(kmfit1, 'Death')
draw_survplot(kmfit2, 'Recurrence')


