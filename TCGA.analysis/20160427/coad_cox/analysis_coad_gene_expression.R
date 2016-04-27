################################################################################
# This scripts generates a Cox PH Model of the relationship between two genes 
#   involved with Colon Adenocarcinoma (COAD)
# Luis Palomero - 2016 04 27
################################################################################


#Dependencies
library(survival)

#Input files
clinical_data <- read.csv('./data/data_clinical_survival.txt', head=T, sep='\t')
expression_data <- read.csv('./data/data_gene_expression_filtered.txt', head=F, sep=' ')

#Normalize and merge both files

normalize_expression_names <- function(name){
  name2 <- substr(name, 0, 12)
  return (gsub("[.]", "-", name2))
}

traspose_and_normalize_expression_data <- function(expression_data){
  rownames(expression_data) =  expression_data[,1]  
  expression_data = expression_data[,-1]
  expression_data = as.data.frame(t(expression_data))
  expression_data[,1] = normalize_expresion_names(expression_data[,1])
  return (expression_data)
}

normalize_events_dataset <- function(dataset){
  keeps = c('event.death', 'event.recur', 'time.death', 'time.recur', 'ACTA1', 'VIM')
  dataset = all_data[,(names(dataset) %in% keeps)]
  dataset = subset(dataset, !(dataset$time.death == '[Not Available]' | dataset$time.recur == '[Not Available]'))
  dataset$time.death = as.numeric(as.character(dataset$time.death))
  dataset$time.recur = as.numeric(as.character(dataset$time.recur))  
  dataset$ACTA1 = as.numeric(as.character(dataset$ACTA1))
  dataset$VIM = as.numeric(as.character(dataset$VIM))
  dataset
}

expression_data = traspose_and_normalize_expression_data(expression_data)

all_data = merge(clinical_data, expression_data, by = 'bcr_patient_barcode' )
all_data = normalize_events_dataset(all_data)

write.table(all_data, file="all_data.txt", sep="\t", row.names=FALSE)

############################
## COX MODELS GENERATION
############################

#Survival models
death.surv = Surv(all_data$time.death, all_data$event.death==1)
recur.surv = Surv(all_data$time.recur, all_data$event.recur==1)

#Event  -> Death
#Factor -> ACTA1
summary(coxph(death.surv ~ ACTA1 , data=all_data))

#Event  -> Recur
#Factor -> ACTA1

summary(coxph(recur.surv ~ ACTA1 , data=all_data))

#Event  -> Death
#Factor -> VIM

summary(coxph(death.surv ~ VIM , data=all_data))

#Event  -> Recur
#Factor -> VIM

summary(coxph(recur.surv ~ VIM , data=all_data))

