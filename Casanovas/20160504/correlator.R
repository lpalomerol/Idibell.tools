#Correlatie 

getwd()

get_invasion_data <- function(filename){
  mRX03 <- read.csv(filename, sep='\t', header=FALSE)  
  invasion <- mRX03[1,]
  invasion  <- (invasion[-1:-3])
  colnames(invasion) <- c('C1','C2','C3', 'DC1','DC2', 'DC3')
  rownames(invasion) <- c('invasion')
  as.numeric(as.matrix(invasion))
}  

get_samples_data <- function(filename){
  file <- read.csv(filename, sep='\t', skip=1, header=TRUE)
  rownames(file) <- file$NAME
  file$C1 = as.numeric(file$C1)
  file$C2 = as.numeric(file$C2)
  file$C3 = as.numeric(file$C3)
  file$DC1 = as.numeric(file$DC1)
  file$DC2 = as.numeric(file$DC2)
  file$DC3 = as.numeric(file$DC3)
  file
}

cor.test.line <- function(gene, invasion){
  gene_name <- as.character(rownames(gene))
  values <- as.numeric(gene[-1:-3])
  the_test <- cor.test(values, invasion)
  c(
    name = gene_name,
    correlation = the_test$estimate,
    p_value = the_test$p.value,
    conf_low = the_test$conf.int[1],
    conf_high = the_test$conf.int[2]
  )
}

###### Correlation for MOUSE data
invasion <- get_invasion_data('./data/mRX03.txt')
mRX03  <- get_samples_data('./data/mRX03.txt')

correlated_genes <- apply(mRX03, 1, cor.test.line, invasion)
correlated_genes = t(correlated_genes)

write.csv(correlated_genes, './output/cor_mRX03.txt')

###### Correlation for HUMAN data
invasion <- get_invasion_data('./data/hRX03.txt')
hRX03  <- get_samples_data('./data/hRX03.txt')

correlated_genes <- apply(hRX03, 1, cor.test.line, invasion)
correlated_genes = t(correlated_genes)

write.csv(correlated_genes, './output/cor_hRX03.txt')


