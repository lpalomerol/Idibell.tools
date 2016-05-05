#Guión
# 1- Cargar los datos de expresión y calcular el subconjunto (30')
# 2- Realizar grupos de BRCA por terciles (5')
# 3- Comprobar las diferencias entre grupos () (en brca_comparator)
#  - Gráficas (boxplots) (15')
#  - Fisher test (low-high) (15')
#  - Bartlett test (all groups) (15')
# 4- Adjuntar los datos clínicos de seguimiento (30')
# 5- Modelos de cox, por cada uno de los subgrupos... 
#  - Modelo con una covariante AREG, incluyendo gráfica de survfit  (25')
#  - Modelo con una covariante AHR, incluyendo gráfica de survfit (10')
#  - Modelo con dos covariantes AHR, incluyendo gráfica de survfit ('10')

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

# 3 - Adjuntar datos de seguimiento
head(expression_data.t)

expression_data.t$barcode = rownames(expression_data.t)
expression_data.t$barcode= substr(expression_data.t$barcode, 0,12)
expression_data.t$barcode= gsub('\\.', '-', expression_data.t$barcode)
expression_data.t

follow_up <- read.csv('./data/brca_followup.txt')
head(follow_up)

ftot = merge(follow_up, expression_data.t, by='barcode')

# 4 - Generar los modelos de Cox
attach(ftot)

ftot.low = ftot[ftot$BRCA1_g==0,]
ftot.low.surv = Surv(ftot.low$time.death, ftot.low$event.death==1)

#MODEL DE COX PER GRUP LOW I GEN AREG
coxph(ftot.low.surv~AREG, data=ftot.low)
#MODEL DE COX PER GRUP LOW I GEN AHR
coxph(ftot.low.surv~AHR, data=ftot.low)

ftot.mid = ftot[ftot$BRCA1_g==1,]
ftot.mid.surv = Surv(ftot.mid$time.death, ftot.mid$event.death==1)
#MODEL DE COX PER GRUP MID I GEN AHR
coxph(ftot.mid.surv~AREG, data=ftot.mid)
#MODEL DE COX PER GRUP MID I GEN AHR
coxph(ftot.mid.surv~AHR, data=ftot.mid)

ftot.high = ftot[ftot$BRCA1_g==2,]
ftot.high.surv = Surv(ftot.high$time.death, ftot.high$event.death==1)
#MODEL DE COX PER GRUP HIGH I GEN AHR
coxph(ftot.high.surv~AREG, data=ftot.high)
#MODEL DE COX PER GRUP HIGH I GEN AHR
coxph(ftot.high.surv~AHR, data=ftot.high)

ftot.surv = Surv(ftot$time.death, ftot$event.death==1)

png('./output/km_brca1.png')
plot(survfit(ftot.surv~ftot$BRCA1_g),
     lty=1:3, mark=3,
     col=c('red','green','blue'),
     main='KM for BRCA1 groups')
legend('bottomleft', c('low','mid','high'),
       col=c('red','green','blue'),
       lty=1:3
 )
dev.off()

help(plot)

coxph(ftot.surv~AREG, data=ftot)
coxph(ftot.surv~AHR, data=ftot)

detach(ftot)

