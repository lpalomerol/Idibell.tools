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

normalize_sample_names <- function(name){
  name2 <- substr(name, 0, 12)
  return (gsub("[.]", "-", name2))
}

traspose_and_normalize_expression_data <- function(expression_data){
  rownames(expression_data) =  expression_data[,1]
  expression_data = expression_data[,-1]
  expression_data = as.data.frame(t(expression_data))
  expression_data[,1] = normalize_sample_names(expression_data[,1])
  return (expression_data)
}

build_patient_expression_dataset <- function(clinical_data, expression_data, fields_to_keep){
  all_data = merge(clinical_data, expression_data, by = 'bcr_patient_barcode' )
  normalize_events_dataset(all_data, fields_to_keep)
}

normalize_events_dataset <- function(dataset, fields_to_keep = BASIC_SURVIVAL_FIELDS){
  my_dataset <- dataset[,(names(dataset) %in% fields_to_keep)]
  suppressWarnings(
    my_dataset[,] <- apply(my_dataset[,], 2, function(x){ as.numeric(as.character(x))})
  )
  my_dataset[complete.cases(my_dataset),]

}