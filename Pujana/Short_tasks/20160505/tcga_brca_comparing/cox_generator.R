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
source('../../../../TCGA.analysis/tools/survival_analysis_tools.R')

# Helping functions

getLoglikP <- function(model_complex,model_simple, degrees){
  LRT = (-2)*(model_simple$loglik[2]-model_complex$loglik[2])
  Pvalue = 1 - pchisq(LRT, degrees)
  Pvalue
}

# 1 - Cargar los datos...

EXPRESSION_DATA_FILE = './data/data_expression_genes_filtered.txt'
expression_data <- load_gene_expression_data(EXPRESSION_DATA_FILE)
cols_to_keep = c('bcr_patient_barcode',  'BRCA1','AREG', 'AHR')
expression_data = expression_data[,colnames(expression_data) %in% cols_to_keep]
expression_data$AREG = as.numeric(as.character(expression_data$AREG))
expression_data$AHR = as.numeric(as.character(expression_data$AHR))
expression_data$BRCA1 = as.numeric(as.character(expression_data$BRCA1))


# 4 - Adjuntar datos de seguimiento (eliminando duplicados)
follow_up <- read.csv('./data/brca_followup.txt')
follow_up = follow_up[!duplicated(follow_up$barcode),]

ftot = merge(follow_up, expression_data, by.x='barcode', by.y='bcr_patient_barcode', all=F)
head(ftot)

# 2 - Crear grupos
tertiles = quantile(ftot$BRCA1, c(0,0.33,0.66,1))
ftot$BRCA1_g = ifelse(ftot$BRCA1< tertiles[2], 0, 
                                 ifelse(ftot$BRCA1< tertiles[3] , 1,2)
)
ftot$BRCA_low_tertile = ifelse(ftot$BRCA1 < tertiles[2], 0,1)

median_brca = median(ftot$BRCA1)
ftot$BRCA_median_g = ifelse(ftot$BRCA1< median_brca, 0, 1)


# 5 - Generar los modelos de supervivencia básicos
# 5.1.1 - Logrank low BRCA1 - med+high BRCA1
# 5.1.2 - Logrank low BRCA1 - med+high BRCA1 ~ AREG 
# 5.1.3 - Logrank low BRCA1 - med+high BRCA1 ~ AHR

death.model <- Surv(ftot$time.death, ftot$event.death==1)
recur.model <- Surv(ftot$time.recur, ftot$event.recur==1)

km_plot <- function(km, title, png_file){
  png(png_file)
  plot(km, col=c('red', 'blue'), lty='solid',
       xlab='Survival time', ylab='Survival probabilities',
       mark=3, main=title
  )
  
  legend('bottomleft', c('Low tertile', 'High tertiles'),
         lty='solid', col=c('red', 'blue')
  )
  dev.off()
}




#LogRank for basic TERTILES group  - Death
survdiff(death.model~ftot$BRCA_low_tertile)
km_plot(survfit(death.model~ftot$BRCA_low_tertile), 
        'Survival time - by tertiles',
        './output/km.survival.tertiles.png')

#LogRank for basic TERTILES group  - Recur
survdiff(recur.model~ftot$BRCA_low_tertile)
km_plot(survfit(recur.model~ftot$BRCA_low_tertile), 
        'Recurrency time - by tertiles',
        './output/km.recurrency.tertiles.png')

#LogRank for basic MEDIAN group  - Death
survdiff(death.model~ftot$BRCA_median_g)
km_plot(survfit(recur.model~ftot$BRCA_median_g), 
        'Survival time - by median',
        './output/km.survival.median.png')

#LogRank for basic MEDIAN group  - Recur
survdiff(recur.model~ftot$BRCA_median_g)
km_plot(survfit(recur.model~ftot$BRCA_median_g), 
        'Recurrency time - by median',
        './output/km.recurrency.median.png')



# 5b - Detectar si existe interacción significativa
#Interacción DEATH - BRCA_tertile - AREG
attach(ftot)

coxph(death.model~BRCA_low_tertile +AREG + BRCA_low_tertile *AREG)
coxph(recur.model~BRCA_low_tertile +AREG + BRCA_low_tertile *AREG)

coxph(death.model~BRCA_median_g +AREG + BRCA_median_g *AREG)
coxph(recur.model~BRCA_median_g +AREG + BRCA_median_g *AREG)

coxph(death.model~BRCA_low_tertile +AHR + BRCA_low_tertile *AHR)
coxph(recur.model~BRCA_low_tertile +AHR + BRCA_low_tertile *AHR)

coxph(death.model~BRCA_median_g +AHR + BRCA_median_g *AHR)
coxph(recur.model~BRCA_median_g +AHR + BRCA_median_g *AHR)

detach(ftot)

# 5c - Detectar si los niveles de expresión de AHR / AREG influyen en el modelo

attach(ftot)
summary(coxph(death.model~BRCA_low_tertile))
summary(coxph(death.model~BRCA_low_tertile +AREG))

summary(coxph(recur.model~BRCA_low_tertile))
summary(coxph(recur.model~BRCA_low_tertile +AREG))

summary(coxph(death.model~BRCA_median_g))
summary(coxph(death.model~BRCA_median_g +AREG))

summary(coxph(recur.model~BRCA_median_g))
summary(coxph(recur.model~BRCA_median_g +AREG))

#----

summary(coxph(death.model~BRCA_low_tertile))
summary(coxph(death.model~BRCA_low_tertile +AHR))


summary(coxph(recur.model~BRCA_low_tertile))
summary(coxph(recur.model~BRCA_low_tertile +AHR))

summary(coxph(death.model~BRCA_median_g))
summary(coxph(death.model~BRCA_median_g +AHR))

summary(coxph(recur.model~BRCA_median_g))
summary(coxph(recur.model~BRCA_median_g +AHR))


detach(ftot)


