library(testthat)
#source('./survival_analysis_tools.R')
context("Normalize_Events_Dataset tests")

test_that("Should keep basic columns when fields_to_keep is not defined", {
  #Arrange
  my_dataframe <- data.frame(
    c(0,1),
    c(0,1),
    c(10,11),
    c(10,10),
    c(12,13)
  )
  row_names <- c('event.death', 'event.recur', 'time.death', 'time.recur', 'another_column')
  names(my_dataframe) <- row_names
  TOTAL_ROWS_AFTER_FILTER <- 2
  ALL_ROWS_FILTERED <- 0
  #Act
  normalized_dataset <- normalize_events_dataset(my_dataframe)

  #Assert
  expect_that(length(normalized_dataset$event.death), equals(TOTAL_ROWS_AFTER_FILTER))
  expect_that(length(normalized_dataset$event.recur), equals(TOTAL_ROWS_AFTER_FILTER))
  expect_that(length(normalized_dataset$time.death), equals(TOTAL_ROWS_AFTER_FILTER))
  expect_that(length(normalized_dataset$time.recur), equals(TOTAL_ROWS_AFTER_FILTER))
  expect_that(length(normalized_dataset$another_column), equals(ALL_ROWS_FILTERED))
})

 test_that("Should override the fields to keep when param fields_to_keep is defined", {
   #Arrange
   my_dataframe <- data.frame(
     c(0,1),
     c(0,1),
     c(10,11),
     c(10,10),
     c(12,13)
   )
   row_names <- c('event.death', 'event.recur', 'time.death', 'time.recur', 'another.event')
   fields_to_keep <- c('event.death', 'another.event')
   names(my_dataframe) <- row_names

   #Act
   normalized_dataset <- normalize_events_dataset(my_dataframe, fields_to_keep)

   #Assert
   expect_that(length(normalized_dataset$event.death), equals(2))
   expect_that(length(normalized_dataset$event.recur), equals(0))
   expect_that(length(normalized_dataset$time.death), equals(0))
   expect_that(length(normalized_dataset$time.recur), equals(0))
   expect_that(length(normalized_dataset$another.event), equals(2))
 })

 test_that('Should cast stringified fields to integer when necessary', {
   #Arrange
   my_dataframe <- data.frame(
     c("0","1"),
     c("0","1"),
     c("10","11"),
     c("10","10"),
     c("12","13")
   )
   row_names <- c('event.death', 'event.recur', 'time.death', 'time.recur', 'another.event')
   names(my_dataframe) <- row_names

   #Act
   normalized_dataset <- normalize_events_dataset(my_dataframe)

   #Assert
   expect_that(normalized_dataset$event.death, is_identical_to(c(0,1)))
   expect_that(normalized_dataset$event.recur, is_identical_to(c(0,1)))
   expect_that(normalized_dataset$time.death, is_identical_to(c(10,11)))
   expect_that(normalized_dataset$time.recur, is_identical_to(c(10,10)))
})

test_that('Should remove rows with invalid fields when they exist', {
   #Arrange
   my_dataframe <- data.frame(
     c("0","1"),
     c("0","1"),
     c("10","LOL"),
     c("10","10"),
     c("12","13")
   )
   row_names <- c('event.death', 'event.recur', 'time.death', 'time.recur', 'another.event')
   names(my_dataframe) <- row_names
   TOTAL_ROWS_AFTER_FILTER <- 1
   #Act
   normalized_dataset <- normalize_events_dataset(my_dataframe)

   #Assert
   expect_that(length(normalized_dataset$event.death), equals(TOTAL_ROWS_AFTER_FILTER))
   expect_that(length(normalized_dataset$event.recur), equals(TOTAL_ROWS_AFTER_FILTER))
   expect_that(length(normalized_dataset$time.death), equals(TOTAL_ROWS_AFTER_FILTER))
   expect_that(length(normalized_dataset$time.recur), equals(TOTAL_ROWS_AFTER_FILTER))

})

context('Filter_control_tissues tests')

test_that('Should remove control tissues from gene expression dataframe', {

  #Arrange
  BARCODE_TUMORIC = 'TCGA-02-0001-01C-01D-0182-01' #Fourth token starts with 0 -> tumor
  BARCODE_NORMAL = 'TCGA-02-0001-11C-01D-0182-01' #Fourth token starts with 1 -> normal
  BARCODE_CONTROL = 'TCGA-02-0001-21C-01D-0182-01' #Fourth token starts with 2 -> control
  gene_expression_sample_dataframe <- data.frame(
    c(BARCODE_TUMORIC, BARCODE_NORMAL, BARCODE_CONTROL),
    c(1.000, 0.5, 1.2),
    c(-1.000, -0.5, 0.3),
    c(0.1, 0.2, 0.3)
  )
  names(gene_expression_sample_dataframe) = c('bcr_patient_barcode', 'GENE1', 'GENE2', 'GENE3')

  #Act
  filtered_gene_expression_sample_dataframe = filter_gene_expression_data(gene_expression_sample_dataframe)
  row_kept = as.character(filtered_gene_expression_sample_dataframe[1,'bcr_patient_barcode'])

  #Assert
  expect_that(ncol(filtered_gene_expression_sample_dataframe), equals(4))
  expect_that(nrow(filtered_gene_expression_sample_dataframe), equals(1))
  expect_that(row_kept, equals(BARCODE_TUMORIC))

})

#Due a bug in the function
test_that('Should take account alphanumeric ids', {
  
  #Arrange
  BARCODE_TUMORIC = 'TCGA-AA-AAAA-01C-01D-0182-01' #Fourth token starts with 0 -> tumor
  BARCODE_NORMAL = 'TCGA-AA-AAAA-11C-01D-0182-01' #Fourth token starts with 1 -> normal
  BARCODE_CONTROL = 'TCGA-AA-AAAA-21C-01D-0182-01' #Fourth token starts with 2 -> control
  gene_expression_sample_dataframe <- data.frame(
    c(BARCODE_TUMORIC, BARCODE_NORMAL, BARCODE_CONTROL),
    c(1.000, 0.5, 1.2),
    c(-1.000, -0.5, 0.3),
    c(0.1, 0.2, 0.3)
  )
  names(gene_expression_sample_dataframe) = c('bcr_patient_barcode', 'GENE1', 'GENE2', 'GENE3')
  
  #Act
  filtered_gene_expression_sample_dataframe = filter_gene_expression_data(gene_expression_sample_dataframe)
  row_kept = as.character(filtered_gene_expression_sample_dataframe[1,'bcr_patient_barcode'])
  
  #Assert
  expect_that(ncol(filtered_gene_expression_sample_dataframe), equals(4))
  expect_that(nrow(filtered_gene_expression_sample_dataframe), equals(1))
  expect_that(row_kept, equals(BARCODE_TUMORIC))
})

context('Remove_dupicated_samples tests')

test_that('Should remove duplicated samples leaving only one when there are repeated patiens',{

  #Arrange  
  BARCODE_DUPE = 'TCGA-02-0001'
  BARCODE_NO_DUPE = 'TCGA-02-0002'
  gene_expression_sample_dataframe <- data.frame(
    c(BARCODE_DUPE, BARCODE_NO_DUPE, BARCODE_DUPE),
    c(1.000, 0.5, 1.2),
    c(-1.000, -0.5, 0.3),
    c(0.1, 0.2, 0.3)
  )
  
  #Act
  filtered_gene_expression_sample_dataframe = remove_duplicated_samples(gene_expression_sample_dataframe)
  row_kept = as.character(filtered_gene_expression_sample_dataframe[,1])
  
  #Assert
  expect_that(ncol(filtered_gene_expression_sample_dataframe), equals(4))
  expect_that(nrow(filtered_gene_expression_sample_dataframe), equals(2))
  expect_that(row_kept, equals(c(BARCODE_DUPE, BARCODE_NO_DUPE)))  
  #Assert
})