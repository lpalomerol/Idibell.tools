#Guión
# 1- Cargar los datos de expresión y calcular el subconjunto (30')
# 2- Realizar grupos de BRCA por terciles (5')
# 3- Comprobar las diferencias entre grupos () (en brca_comparator)
#  - Gráficas (boxplots) (15')
#  - Fisher test (low-high) (15')
#  - Bartlett test (all groups) (15')
# 4- Adjuntar los datos clínicos de seguimiento (30')
# 5- Crear logrank test dels dos grups extrems de BRCA, tenint en conte ...
#  les covariants

library(survival)

# 1 - Cargar los datos...

expression_data <- read.csv('./data/data_expression_genes_filtered.txt', sep=' ',head=TRUE)
rownames(expression_data) <- expression_data$bcr_patient_barcode
genes_to_keep = c('BRCA1','AREG', 'AHR')
expression_data = expression_data[expression_data$bcr_patient_barcode %in% genes_to_keep, ]
expression_data.t = as.data.frame(t(expression_data[,-1]))

# 2 - Crear grupos
quantile(expression_data.t$BRCA1, c(0,0.33,0.66,1))
expression_data.t$BRCA1_g = ifelse(expression_data.t$BRCA1< -1.8644033  , 0, 
                                   ifelse(expression_data.t$BRCA1< -1.2340033 , 1,2)
)

# 4 - Adjuntar datos de seguimiento
head(expression_data.t)

#Normalizar los barcodes
expression_data.t$barcode = rownames(expression_data.t)
expression_data.t$barcode= substr(expression_data.t$barcode, 0,12)
expression_data.t$barcode= gsub('\\.', '-', expression_data.t$barcode)
expression_data.t

follow_up <- read.csv('./data/brca_followup.txt')
head(follow_up)

ftot = merge(follow_up, expression_data.t, by='barcode', all=F)

# 5 - Generar los modelos de Cox -> LOGRANK tests

model <- Surv(data$time.death, data$event.death==1)

getLoglikP <- function(model_complex,model_simple, degrees){
  LRT = (-2)*(model_simple$loglik[2]-model_complex$loglik[2])
  Pvalue = 1 - pchisq(LRT, degrees)
  Pvalue
}

generate_survivals <- function(model, data){
 
  list(
    km = survfit(model~data$BRCA1_g),
    diff.base =  survdiff(model~data$BRCA1_g),
    cox.base =  coxph(model~data$BRCA1_g),
    cox.areg = coxph(model~data$BRCA1_g+data$AREG),
    cox.ahr =  coxph(model~data$BRCA1_g+data$AHR),
    cox.areg_ahr = coxph(model~data$BRCA1_g + data$AHR + data$AREG),
    cox.interaction = coxph(model~data$BRCA1_g + data$AHR + data$AREG  + 
                              data$BRCA1_g * data$AHR + data$BRCA1_g * data$AREG )
  )

}

ftot.extreme = ftot[ftot$BRCA1_g != 1,]

model <- Surv(ftot.extreme$time.death, ftot.extreme$event.death==1)
(ftot.ex.death.surv <- generate_survivals(model, ftot.extreme))

model <- Surv(ftot.extreme$time.recur, ftot.extreme$event.recur==1)
ftot.ex.recur.surv <- generate_survivals(model, ftot.extreme)

model <- Surv(ftot$time.death, ftot$event.death==1)
ftot.death.surv <- generate_survivals(model, ftot)

model <- Surv(ftot$time.recur, ftot$event.recur==1)
ftot.recur.surv <- generate_survivals(model, ftot)

(ftot.ex.death.surv)

getLoglikP(ftot.ex.death.surv$cox.interaction, ftot.ex.death.surv$cox.areg_ahr, 2)
getLoglikP(ftot.ex.recur.surv$cox.interaction, ftot.ex.recur.surv$cox.areg_ahr, 2)



summary(ftot.ex.recur.surv$cox.ahr)
