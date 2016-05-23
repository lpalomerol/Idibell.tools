
ROOT_DIRECTORY = '../../..'
DATA_FOLDER_DIRECTORY = './data'

source(
  paste(ROOT_DIRECTORY, '/TCGA.analysis/tools/survival_analysis_tools.R', sep='')
)

min_max_by_tertile <- function(dataset, row, min_tertile, max_tertile){
  dataset[(dataset[row] >= min_tertile & dataset[row] <= max_tertile), ]  
}

tertilize <- function(dataset, column){
  FIRST_TERTILE = 2
  SECOND_TERTILE = 3
  THIRD_TERTILE = 4
  
  tertiles = quantile(dataset[[column]], c(0, 0.33, 0.66, 1))
  ifelse(dataset[column] < tertiles[FIRST_TERTILE], 
         0, 
         ifelse(dataset[column] < tertiles[SECOND_TERTILE], 1, 2))  
}

expression_data_file <-   paste(DATA_FOLDER_DIRECTORY, '/data_expression_genes.txt', sep='')
expression_data <- load_gene_expression_data(expression_data_file,sep=' ')

rownames(expression_data) <- expression_data$bcr_patient_barcode
expression_data$AHR = as.numeric(as.character(expression_data$AHR))
expression_data$AREG = as.numeric(as.character(expression_data$AREG))
expression_data$BRCA1 = as.numeric(as.character(expression_data$BRCA1))
expression_data$BRCA2 = as.numeric(as.character(expression_data$BRCA2))
head(expression_data)

ZERO_TERTILE = 1
FIRST_TERTILE = 2
SECOND_TERTILE = 3
THIRD_TERTILE = 4
brca1_tertiles = quantile(expression_data$BRCA1, c(0, 0.33, 0.66, 1))
brca2_tertiles = quantile(expression_data$BRCA2, c(0, 0.33, 0.66, 1))

expression_data$BRCA1_tertiles = tertilize(expression_data, 'BRCA1')
expression_data$BRCA2_tertiles = tertilize(expression_data, 'BRCA2')


write.table(expression_data, './output/expression_data_tertiles.txt')

print('BRCA1 FIRST TERTILE FIVENUM')
fivenum(min_max_by_tertile(expression_data, 'BRCA1', brca1_tertiles[ZERO_TERTILE][[1]], brca1_tertiles[FIRST_TERTILE][[1]])$BRCA1)

print('BRCA1 SECOND TERTILE FIVENUM')
fivenum(min_max_by_tertile(expression_data, 'BRCA1', brca1_tertiles[FIRST_TERTILE][[1]], brca1_tertiles[SECOND_TERTILE][[1]])$BRCA1)

print('BRCA1 THIRD TERTILE FIVENUM')
fivenum(min_max_by_tertile(expression_data, 'BRCA1', brca1_tertiles[SECOND_TERTILE][[1]], brca1_tertiles[THIRD_TERTILE][[1]])$BRCA1)

print('BRCA2 FIRST TERTILE FIVENUM')
fivenum(min_max_by_tertile(expression_data, 'BRCA2', brca2_tertiles[ZERO_TERTILE][[1]], brca2_tertiles[FIRST_TERTILE][[1]])$BRCA2)

print('BRCA2 SECOND TERTILE FIVENUM')
fivenum(min_max_by_tertile(expression_data, 'BRCA2', brca2_tertiles[FIRST_TERTILE][[1]], brca2_tertiles[SECOND_TERTILE][[1]])$BRCA2)

print('BRCA2 THIRD TERTILE FIVENUM')
fivenum(min_max_by_tertile(expression_data, 'BRCA2', brca2_tertiles[SECOND_TERTILE][[1]], brca2_tertiles[THIRD_TERTILE][[1]])$BRCA2)

attach(expression_data)

split(AREG, expression_data$BRCA1_tertiles)


postscript('./output/brca1_areg.ps')
boxplot(AREG~expression_data$BRCA1_tertiles, main="AREG ~ BRCA1 tertiles",
        xlab='BRCA1 group', ylab='AREG expression level',
        names=c('Low', 'Mid', 'High'))
dev.off()

postscript('./output/brca1_ahr.ps')
boxplot(AHR~expression_data$BRCA1_tertiles, main="AHR ~ BRCA1 tertiles",
        xlab='BRCA1 group', ylab='AHR expression level',
        names=c('Low', 'Mid', 'High'))
dev.off()

postscript('./output/bcra2_areg.ps')
boxplot(AREG~expression_data$BRCA2_tertiles, main="AREG ~ BRCA2 tertiles",
        xlab='BRCA2 group', ylab='AREG expression level',
        names=c('Low', 'Mid', 'High'))
dev.off()

postscript('./output/brca2_ahr.ps')
boxplot(AHR~expression_data$BRCA2_tertiles, main="AHR ~ BRCA2 tertiles",
        xlab='BRCA2 group', ylab='AHR expression level',
        names=c('Low', 'Mid', 'High'))
dev.off()

t.test(
  expression_data[BRCA1_tertiles==0, 'AREG'],
  expression_data[BRCA1_tertiles==1, 'AREG']
)

t.test(
  expression_data[BRCA1_tertiles==0, 'AREG'],
  expression_data[BRCA1_tertiles==2, 'AREG']
)

t.test(
  expression_data[BRCA1_tertiles==1, 'AREG'],
  expression_data[BRCA1_tertiles==2, 'AREG']
)


t.test(
  expression_data[BRCA2_tertiles==0, 'AREG'],
  expression_data[BRCA2_tertiles==1, 'AREG']
)


t.test(
  expression_data[BRCA2_tertiles==0, 'AREG'],
  expression_data[BRCA2_tertiles==2, 'AREG']
)

t.test(
  expression_data[BRCA2_tertiles==1, 'AREG'],
  expression_data[BRCA2_tertiles==2, 'AREG']
)



####################


t.test(
  expression_data[BRCA1_tertiles==0, 'AHR'],
  expression_data[BRCA1_tertiles==1, 'AHR']
)

t.test(
  expression_data[BRCA1_tertiles==0, 'AHR'],
  expression_data[BRCA1_tertiles==2, 'AHR']
)

t.test(
  expression_data[BRCA1_tertiles==1, 'AHR'],
  expression_data[BRCA1_tertiles==2, 'AHR']
)


t.test(
  expression_data[BRCA2_tertiles==0, 'AHR'],
  expression_data[BRCA2_tertiles==1, 'AHR']
)


t.test(
  expression_data[BRCA2_tertiles==0, 'AHR'],
  expression_data[BRCA2_tertiles==2, 'AHR']
)

t.test(
  expression_data[BRCA2_tertiles==1, 'AHR'],
  expression_data[BRCA2_tertiles==2, 'AHR']
)




detach(expression_data)
