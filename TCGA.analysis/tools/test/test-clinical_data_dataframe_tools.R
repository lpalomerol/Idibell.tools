library(testthat)

context('Get_death_time tests')
# source('./clinical_data_dataframe_tools.R')

DAYS_DEATH = 10
DAYS_LAST_CONTACT= 40
MONTHS_DEATH = 0
MONTHS_LAST_CONTACT = 1
EVENT_DEATH = 1
NO_EVENT_DEATH = 0

test_that('Should decide "death time event" when the event death is TRUE', {
  #Arrange

  line_to_check = data.frame(
    c(EVENT_DEATH),
    c(DAYS_DEATH),
    c(DAYS_LAST_CONTACT)
  )
  names(line_to_check) = c('event.death', 'death_days_to', 'last_contact_days_to')

  #Act
  days_found = get_line_death_time(line_to_check)

  #Assert
  expect_that(days_found, equals(DAYS_DEATH))
})


test_that('Should decide "last_contact_days_to" when the event death is FALSE', {
  #Arrange
  line_to_check = data.frame(
    c(NO_EVENT_DEATH),
    c(DAYS_DEATH),
    c(DAYS_LAST_CONTACT)
  )
  names(line_to_check) = c('event.death', 'death_days_to', 'last_contact_days_to')

  #Act
  days_found = get_line_death_time(line_to_check)


  #Assert
  expect_that(days_found, equals(DAYS_LAST_CONTACT))
})

context('Get_death_time')

test_that('Should make floor to each death time found', {
  #Arrange
  line_to_check = data.frame(
    c(EVENT_DEATH, NO_EVENT_DEATH),
    c(DAYS_DEATH, DAYS_DEATH),
    c(DAYS_LAST_CONTACT, DAYS_LAST_CONTACT)
  )
  names(line_to_check) = c('event.death', 'death_days_to', 'last_contact_days_to')
  
  #Act
  days_found = get_death_time(line_to_check)
  #Assert
  expect_that(days_found, equals(c(MONTHS_DEATH, MONTHS_LAST_CONTACT)))
})

