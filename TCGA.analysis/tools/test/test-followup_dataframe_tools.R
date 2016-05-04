

if (!require(testthatsomemore)) {
  if (!require(devtools)) install.packages('devtools'); require(devtools)
  install_github('robertzk/testthatsomemore') 
}

#source('../followup_dataframe_tools.R')

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

context('Normalize_followup_dataframe tests')

build_sample_dataframe <- function(){
  complete_dataframe = data.frame(
    c('uid1', 'uid2'),
    c('barcode1', 'barcode2'),
    c('10','22'),
    c('11', '12'),
    c('Alive', 'Dead'),
    c('13', '14')
  )
  names(complete_dataframe) = c('bcr_patient_uuid', 'bcr_patient_barcode', 
                                'new_tumor_event_dx_days_to', 'last_contact_days_to', 
                                'vital_status', 'death_days_to')  
  complete_dataframe
}

test_that('Should keep base columns with complete followup dataframe provided', {

  #Arrange
  complete_dataframe = build_sample_dataframe()
  
  #Act
  normalized_dataframe = normalize_followup_dataframe(complete_dataframe)

  #Assert
  expect_that(length(normalized_dataframe$barcode), equals(2))
  expect_that(length(normalized_dataframe$last), equals(2))
  expect_that(length(normalized_dataframe$death), equals(2))
  expect_that(length(normalized_dataframe$recur), equals(2))
  expect_that(length(normalized_dataframe$bcr_followup_barcode), equals(2))
  
})


context('Build_followup_file_v4_0 tests')

test_that('Should merge the two dataframes', {
  
  #Arrange
  followup_name = 'followup'
  followup_nte_name = 'nte'
  
  stub(build_followup_file_v4_0, 'build_followup_file') <- function(file){
    if(file == 'followup'){
      df = data.frame(
        c('uuid1', 'uuid2'),
        c('barcode1', 'barcode2'),
        c('Alive', 'Dead'),
        c('10', '11'),
        c('12', '13')
      )
      names(df) <- c('bcr_patientuuid', 'bcr_patient_barcode', 'vital_status', 'last_contact_days_to', 'death_days_to')
    } else { # New time Event
      df = data.frame(
        c('uuid1', 'uuid2'),
        c('barcode1', 'barcode2'),
        c('12', '13')
      )
      names(df) <- c('bcr_patientuuid', 'bcr_patient_barcode', 'new_tumor_event_dx_days_to')
    }
    df
  }
  #Act
  df <- build_followup_file_v4_0(followup_name, followup_nte_name)
  #Assert
  expect_that(length(df$bcr_patient_barcode), equals(2))
  expect_that(length(df$vital_status), equals(2))
  expect_that(length(df$last_contact_days_to), equals(2))
  expect_that(length(df$new_tumor_event_dx_days_to), equals(2))
  expect_that(length(df$death_days_to), equals(2))  
})


test_that('Patients without entry at NTE should be kept in the matching', {
  
  #Arrange
  followup_name = 'followup'
  followup_nte_name = 'nte'
  
  stub(build_followup_file_v4_0, 'build_followup_file') <- function(file){
    if(file == 'followup'){
      df = data.frame(
        c('uuid1', 'uuid2'),
        c('barcode1', 'barcode2'),
        c('Alive', 'Dead'),
        c('10', '11'),
        c('[Not Available]', '11')
      )
      names(df) <- c('bcr_patientuuid', 'bcr_patient_barcode', 'vital_status', 
                     'last_contact_days_to', 'death_days_to')
    } else { # New time Event
      df = data.frame(
        c('uuid1'),
        c('barcode1'),
        c('12')
      )
      names(df) <- c('bcr_patientuuid', 'bcr_patient_barcode', 'new_tumor_event_dx_days_to')
    }
    df
  }
  #Act
  df <- build_followup_file_v4_0(followup_name, followup_nte_name)
  #Assert
  expect_that(length(df$bcr_patient_barcode), equals(2))
  expect_that(length(df$vital_status), equals(2))
  expect_that(df$last_contact_days_to, equals(factor(c('10', '11'))))
  expect_that(df$new_tumor_event_dx_days_to, equals(factor(c('12', NA))))
  expect_that(df$death_days_to, equals(factor(c('[Not Available]', '11'))))
  })

#Filtering responsability at another task.
test_that('Patients with duplicated entries at NTE should be kept in the matching', {
  
  #Arrange
  followup_name = 'followup'
  followup_nte_name = 'nte'
  
  stub(build_followup_file_v4_0, 'build_followup_file') <- function(file){
    if(file == 'followup'){
      df = data.frame(
        c('uuid1'),
        c('barcode1'),
        c('Alive'),
        c('10')
      )
      names(df) <- c('bcr_patientuuid', 'bcr_patient_barcode', 'vital_status', 'last_contact_days_to')
    } else { # New time Event
      df = data.frame(
        c('uuid1', 'uuid1'),
        c('barcode1', 'barcode1'),
        c('12', '13')
      )
      names(df) <- c('bcr_patientuuid', 'bcr_patient_barcode', 'new_tumor_event_dx_days_to')
    }
    df
  }
  #Act
  df <- build_followup_file_v4_0(followup_name, followup_nte_name)
  #Assert
  expect_that(df$bcr_patient_barcode, equals(factor(c('barcode1', 'barcode1'))))
  expect_that(length(df$vital_status), equals(2))
  expect_that(length(df$last_contact_days_to), equals(2))
  expect_that(length(df$new_tumor_event_dx_days_to), equals(2))
})



context('Load_followup_data_file tests')

test_that('Should throw error when version is not defined', {
  expect_that(load_follow_up_data(''), throws_error('Undefined version'))
})

test_that('Should throw error when version is invalid', {
  expect_that(load_follow_up_data('', version = 'invalid'), throws_error('Invalid version'))
})

test_that('Should load and filter v1.5 follow up data successfully',{
  #Arrange
  stub(load_follow_up_data, 'build_followup_file') <- function(file){build_sample_dataframe()}  
  sample_filename = 'sample.txt'
  #Act
  df <- load_follow_up_data(sample_filename, v1_5)
  #Assert
  expect_that(length(df$barcode), equals(2))
})

test_that('Should load and filter v2.1 follow up data successfully',{
  #Arrange
  stub(load_follow_up_data, 'build_followup_file') <- function(file){build_sample_dataframe()}  
  sample_filename = 'sample.txt'
  #Act
  df <- load_follow_up_data(sample_filename, v2_1)
  #Assert
  expect_that(length(df$barcode), equals(2))
})



test_that('Should load and filter v4.0 follow up data successfully',{
  #Arrange
  stub(load_follow_up_data, 'build_followup_file_v4_0') <- function(file, file_nte){build_sample_dataframe()}  
  sample_filename = 'sample.txt'
  #Act
  df <- load_follow_up_data(sample_filename, v4_0)
  #Assert
  expect_that(length(df$barcode), equals(2))
})
