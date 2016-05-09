require(testthat)

context('Transpose_and_normalize_expression_data - INTEGRATION TESTS')

test_that('Should transpose and cut patients data names - INTEGRATION', {
  #Arrange
  BARCODE_HEADER = 'brc_patient_barcode'
  GENE1 = 'ABCD'
  GENE2 = 'EFGH'
  GENE3 = 'IJK_L'
  BARCODE1 = 'TCGA-02-0001-01C-01D-0182-01'
  BARCODE2 = 'TCGA-02-0002-01C-01D-0182-01'
  BARCODE1_short = substr(BARCODE1, 0,12)
  BARCODE2_short = substr(BARCODE2, 0,12)
  raw_expression = data.frame(
    c(BARCODE_HEADER, GENE1, GENE2, GENE3),
    c(BARCODE1, 1,2,3),
    c(BARCODE2, 2,4,6)
  )

  #Act
  normalized_expression = traspose_and_normalize_expression_data(raw_expression)
  #Assert
  expect_that(ncol(normalized_expression), equals(4)) # Sample code plus 3 genes
  expect_that(nrow(normalized_expression), equals(2)) # 2 samples
  expect_that(normalized_expression$brc_patient_barcode, equals(c(BARCODE1_short, BARCODE2_short)))
})

test_that('Should ignore non cancer tissues - INTEGRATION', {
  BARCODE_HEADER = 'brc_patient_barcode'
  GENE1 = 'ABCD'
  GENE2 = 'EFGH'
  GENE3 = 'IJK_L'
  BARCODE_CANCER = 'TCGA-02-0001-01C-01D-0182-01'
  BARCODE_CONTROL = 'TCGA-02-0001-11C-01D-0182-01'
  BARCODE_CANCER_short = substr(BARCODE_CANCER, 0,12)
  raw_expression = data.frame(
    c(BARCODE_HEADER, GENE1, GENE2, GENE3),
    c(BARCODE_CONTROL, 2,4,6),
    c(BARCODE_CANCER, 1,2,3)
  )

  #Act
  normalized_expression = traspose_and_normalize_expression_data(raw_expression)
  #Assert..

  expect_that(ncol(normalized_expression), equals(4)) # Sample code plus 3 genes
  expect_that(nrow(normalized_expression), equals(1)) # 1 sample, removes cancer
  expect_that(normalized_expression$brc_patient_barcode, equals(c(BARCODE_CANCER_short)))
})

test_that('Should remove duplicated values - INTEGRATION', {
  BARCODE_COLUMN = 1
  BARCODE_HEADER = 'brc_patient_barcode'
  GENE1 = 'ABCD'
  GENE2 = 'EFGH'
  GENE3 = 'IJK_L'
  BARCODE_DUPE = 'TCGA-02-0001-01C-01D-0182-01'
  BARCODE_DUPE_2 = 'TCGA-02-0001-01C-01D-0182-02'
  BARCODE_NO_DUPE = 'TCGA-02-0002-01C-01D-0182-01'
  BARCODE_DUPE_short = substr(BARCODE_DUPE, 0,12)
  BARCODE_NO_DUPE_short = substr(BARCODE_NO_DUPE, 0,12)
  raw_expression = data.frame(
    c(BARCODE_HEADER, GENE1, GENE2, GENE3),
    c(BARCODE_DUPE, 2,4,6),
    c(BARCODE_DUPE_2, 2.5,4.4,6.3),
    c(BARCODE_NO_DUPE, 1,2,3)
  )
  
  #Act
  normalized_expression = traspose_and_normalize_expression_data(raw_expression)
  
  #Assert..
  expect_that(ncol(normalized_expression), equals(4)) 
  expect_that(nrow(normalized_expression), equals(2)) 
  expect_that(normalized_expression[,BARCODE_COLUMN], equals(c(BARCODE_DUPE_short, BARCODE_NO_DUPE_short))) 
})