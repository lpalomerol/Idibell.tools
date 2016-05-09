#Constants
BASIC_SURVIVAL_FIELDS = c(
  c('event.death', 'event.recur', 'time.death', 'time.recur')
)

#Functions

load_clinical_data <- function(location){
  read.csv(location, head=T, sep='\t')
}

load_gene_expression_data <- function(location){
  expression_data <- read.csv(location, head=F, sep=' ')
  traspose_and_normalize_expression_data(expression_data)

}

get_clinical_and_expression_data <- function(filename_clinical, filename_expression, gene_name){
  clinical_data <- load_clinical_data(filename_clinical)
  expression_data <- load_gene_expression_data(filename_expression)
  patient_data_fields <- append(BASIC_SURVIVAL_FIELDS, gene_name)
  build_patient_expression_dataset(clinical_data, expression_data, patient_data_fields)
}


normalize_sample_names <- function(name){
  name2 <- substr(name, 0, 12)
  return (gsub("[.]", "-", name2))
}

traspose_and_normalize_expression_data <- function(expression_data){
  rownames(expression_data) =  expression_data[,1]
  expression_data = expression_data[,-1]
  expression_data = as.data.frame(t(expression_data))
  expression_data = filter_gene_expression_data(expression_data)
  expression_data[,1] = normalize_sample_names(expression_data[,1])
  expression_data = remove_duplicated_samples(expression_data)

  return (expression_data)
}

filter_gene_expression_data <- function(gene_expression_data){
  BARCODE_COLUMN = 1   
  tumor_columns =as.character(gene_expression_data[,BARCODE_COLUMN,])
  filtered = grepl("TCGA-[[:alnum:]]{2}-[[:alnum:]]{4}-0[0-9]*", tumor_columns)
  gene_expression_data[filtered,]
}

remove_duplicated_samples <- function(gene_expression_data) {
  BARCODE_COLUMN = 1
  gene_expression_data[!duplicated(gene_expression_data[, BARCODE_COLUMN]),]
}

build_patient_expression_dataset <- function(clinical_data, expression_data, fields_to_keep){
  all_data = merge(clinical_data, expression_data, by.y = 'bcr_patient_barcode', by.x='barcode' )
  normalize_events_dataset(all_data, fields_to_keep)
}

normalize_events_dataset <- function(dataset, fields_to_keep = BASIC_SURVIVAL_FIELDS){
  my_dataset <- dataset[,(names(dataset) %in% fields_to_keep)]
  suppressWarnings(
    my_dataset[,] <- apply(my_dataset[,], 2, function(x){ as.numeric(as.character(x))})
  )
  my_dataset[complete.cases(my_dataset),]

}

build_surv_models <- function(dataframe){
  list(
    death = Surv(dataframe$time.death, dataframe$event.death==1),
    recur = Surv(dataframe$time.recur, dataframe$event.recur==1)
  )
}
