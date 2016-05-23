#Gui?n
# 1- Cargar los datos de expresi?n y calcular el subconjunto (30')
# 2- Realizar grupos de BRCA por terciles (5')
# 3- Comprobar las diferencias entre grupos ()
#  - Gr?ficas (boxplots) (15')
#  - Fisher test (low-high) (15')
#  - Bartlett test (all groups) (15')
# (los siguientes en cox_generator.R)
# 4- Adjuntar los datos cl?nicos de seguimiento (30') 
# 5- Modelos de cox, por cada uno de los subgrupos... 
#  - Modelo con una covariante AREG, incluyendo gr?fica de survfit  (25')
#  - Modelo con una covariante AHR, incluyendo gr?fica de survfit (10')
#  - Modelo con dos covariantes AHR, incluyendo gr?fica de survfit ('10')

# 1 - Cargar los datos...
setwd('GitHub/Idibell.tools/Pujana/Short_tasks/20160505/tcga_brca_comparing/')
expression_data <- read.csv('./data/data_expression_genes_filtered.txt', sep=' ',head=TRUE)
rownames(expression_data) <- expression_data$bcr_patient_barcode
genes_to_keep = c('BRCA1','AREG', 'AHR', 'BRCA2')
expression_data = expression_data[expression_data$bcr_patient_barcode %in% genes_to_keep, ]
expression_data.t = as.data.frame(t(expression_data[,-1]))

# 2 - Crear grupos
quantile(expression_data.t$BRCA1, c(0,0.33,0.66,1))
expression_data.t$BRCA1_g = ifelse(expression_data.t$BRCA1< -1.8644033  , 0, 
                                   ifelse(expression_data.t$BRCA1< -1.2340033 , 1,2)
)
table(expression_data.t$BRCA1_g)

quantile(expression_data.t$BRCA2, c(0,0.33,0.66,1))
expression_data.t$BRCA2_g = ifelse(expression_data.t$BRCA2< -1.39207  , 0, 
                                   ifelse(expression_data.t$BRCA2< -0.58697 , 1,2)
)
table(expression_data.t$BRCA2_g)
split(expression_data.t$BRCA2,expression_data.t$BRCA2_g)
View(expression_data.t)

rownames(expression_data.t)

attach(expression_data.t)


View(expression_data.t)



png('./output/boxplot_areg.png')
boxplot(AREG~BRCA1_g, main="Boxplot AREG expression ~ BRCA1 grouped",
        xlab='BRCA1 group', ylab='AREG expression level',
        names=c('Low', 'Mid', 'High'))
dev.off()

png('./output/boxplot_ahr.png')
boxplot(AHR~BRCA1_g, main="Boxplot AHR expression ~ BRCA1 grouped",
        xlab='BRCA1 group', ylab='AHR expression level',
        names=c('Low', 'Mid', 'High'))
dev.off()

boxplot(AREG~BRCA2_g, main="Boxplot AREG expression ~ BRCA2 grouped VIEJO",
        xlab='BRCA1 group', ylab='AREG expression level',
        names=c('Low', 'Mid', 'High'))

boxplot(AHR~BRCA2_g, main="Boxplot AHR expression ~ BRCA2 grouped VIEJO",
        xlab='BRCA1 group', ylab='AHR expression level',
        names=c('Low', 'Mid', 'High'))


t.test(
  expression_data.t[expression_data.t$BRCA1_g==0, 'AREG'],
  expression_data.t[expression_data.t$BRCA1_g==1, 'AREG']
)

t.test(
  expression_data.t[expression_data.t$BRCA1_g==0, 'AREG'],
  expression_data.t[expression_data.t$BRCA1_g==2, 'AREG']
)

t.test(
  expression_data.t[expression_data.t$BRCA1_g==1, 'AREG'],
  expression_data.t[expression_data.t$BRCA1_g==2, 'AREG']
)


t.test(
  expression_data.t[expression_data.t$BRCA1_g==0, 'AHR'],
  expression_data.t[expression_data.t$BRCA1_g==1, 'AHR']
)

t.test(
  expression_data.t[expression_data.t$BRCA1_g==0, 'AHR'],
  expression_data.t[expression_data.t$BRCA1_g==2, 'AHR']
)

t.test(
  expression_data.t[expression_data.t$BRCA1_g==1, 'AHR'],
  expression_data.t[expression_data.t$BRCA1_g==2, 'AHR']
)

bartlett.test(expression_data.t$AREG ~ expression_data.t$BRCA1_g)
bartlett.test(expression_data.t$AHR ~ expression_data.t$BRCA1_g)

shapiro.test(BRCA1)

png('./output/qqplot-brca1.png')
qqnorm(BRCA1, main='qq-plot BRCA1')
qqline(BRCA1)
dev.off()

shapiro.test(AREG)

png('./output/qqplot-areg.png')
qqnorm(AREG, main='qq-plot AREG')
qqline(AREG)
dev.off()

shapiro.test(AHR)

png('./output/qqplot-ahr.png')
qqnorm(AHR, main='qq-plot AHR')
qqline(AHR)
dev.off()

detach(expression_data.t)
