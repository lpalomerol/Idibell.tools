library(testthat)

ASSETS_DIR = './assets'
TMP_OUTPUT_DIR = './tmp'

gct.loader = function(file){
  raw = readLines(file)
  #Remove unwanted headers
  raw = raw[-1:-2]
  read.csv(textConnection(raw), header = TRUE, stringsAsFactors = FALSE, sep='\t')
}


context('expression.to_cgt.R tests')

test_that("Should store expression data in tab format", {
  #Arrange
  DATASET_COLUMNS = 174
  TOTAL_COLUMNS = 1+1+DATASET_COLUMNS
  expression_file = paste(ASSETS_DIR, '/gene_expression.txt', sep="")
  expression = read.csv(expression_file, sep=' ')
  output_path = paste(TMP_OUTPUT_DIR, '/gene_expression_tab.gct', sep="" )
  
  #Act
  expression.to_cgt(expression, output_path)
  
  #Assert
  cgt_expression = gct.loader(output_path)
  expect_that(ncol(cgt_expression), equals(TOTAL_COLUMNS))

})

test_that("Should store version in first line", {
  FIRST_LINE = 1
  expression_file = paste(ASSETS_DIR, '/gene_expression.txt', sep="")
  expression = read.csv(expression_file, sep=' ')
  output_path = paste(TMP_OUTPUT_DIR, '/gene_expression_header_first.gct', sep="" )
  
  #Act
  expression.to_cgt(expression, output_path)
  
  #Assert
  cgt_expression = readLines(output_path)
  expect_that(cgt_expression[FIRST_LINE], equals('#1.2'))

})

test_that("Should store number of lines at second line", {
  SECOND_LINE = 2
  expression_file = paste(ASSETS_DIR, '/gene_expression.txt', sep="")
  expression = read.csv(expression_file, sep=' ')
  output_path = paste(TMP_OUTPUT_DIR, '/gene_expression_header_snd.gct', sep="" )
  
  #Act
  expression.to_cgt(expression, output_path)
  
  #Assert
  cgt_expression = readLines(output_path)
  expect_that(cgt_expression[SECOND_LINE], equals('2\t174'))
})

test_that("Should store name-description-sample_ids info at third line",{
  THIRD_LINE = 3
  expression_file = paste(ASSETS_DIR, '/gene_expression.txt', sep="")
  expression = read.csv(expression_file, sep=' ')
  output_path = paste(TMP_OUTPUT_DIR, '/gene_expression_header_names.gct', sep="" )
  
  #Act
  expression.to_cgt(expression, output_path)
  
  #Assert
  cgt_expression = readLines(output_path)
  expect_that(cgt_expression[THIRD_LINE], matches('NAME\tDescription\tTCGA.AA'))
})