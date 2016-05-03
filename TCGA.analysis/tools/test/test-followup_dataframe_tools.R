library(testthat)
context('Clean_followup_dataframe_data')

test_that('Should make the column numeric when is string', {
  #Arrange
  dataframe = data.frame(
    c('A1', 'A2'),
    c('10', '12'),
    c('uid1', 'uid2')
  )
  names(dataframe) <- c('bcr_patient_barcode', 'new_tumor_event_dx_days_to', 'bcr_patient_uuid')  
  
  #Act
  clean_dataframe <- clean_follow_up_dataframe_data(dataframe)
  
  #Assert
  expect_that(clean_dataframe$new_tumor_event_dx_days_to, equals(c(10, 12)))
})

test_that('Should filter repeated values, removing repetaed ...', {
  #Arrange
  dataframe = data.frame(
    c('A1', 'A2', 'A1'),
    c('10', '12', '6'),
    c('uid1', 'uid2')
  )
  names(dataframe) <- c('bcr_patient_barcode', 'new_tumor_event_dx_days_to', 'bcr_patient_uuid')  
  #Act
  
  #Assert
})