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

context('Get_death_time tests')

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

context('Check_line_is_dead tests')

VITAL_STATUS_ALIVE = 'Alive'
VITAL_STATUS_DEAD = 'Dead'

test_that('When the patient is DEAD should return "1" as event value', {
  #Arrange 
  line_to_check = data.frame(
    c(VITAL_STATUS_DEAD)
  )
  names(line_to_check) = c('vital_status')
  
  #Act
  vital_status_found = check_line_is_dead(line_to_check)
  
  #Arrange
  expect_that(vital_status_found, equals(EVENT_DEATH))
})

test_that('When the patient is ALIVE should return "1" as event value', {
  #Arrange 
  line_to_check = data.frame(
    c(VITAL_STATUS_ALIVE)
  )
  names(line_to_check) = c('vital_status')
  
  #Act
  vital_status_found = check_line_is_dead(line_to_check)
  
  #Arrange
  expect_that(vital_status_found, equals(NO_EVENT_DEATH))
})

context('Attach_events_data tests')

test_that('Should add death events data when valid dataframe is provided', {
  #Arrange 
  line_to_check = data.frame(
    c(VITAL_STATUS_ALIVE, VITAL_STATUS_DEAD),
    c(DAYS_DEATH, DAYS_DEATH),
    c(DAYS_LAST_CONTACT, DAYS_LAST_CONTACT)
  )
  names(line_to_check) = c('vital_status','death_days_to', 'last_contact_days_to')
  
  #Act
  death_events_data = attach_events_data(line_to_check)
  #Arrange
  expect_that(toString(death_events_data$vital_status[1]), equals(VITAL_STATUS_ALIVE))
  expect_that(toString(death_events_data$vital_status[2]), equals(VITAL_STATUS_DEAD))
    expect_that(death_events_data$event.death, equals(c(NO_EVENT_DEATH, EVENT_DEATH)))
  expect_that(death_events_data$time.death, equals(c(MONTHS_LAST_CONTACT, MONTHS_DEATH)))
})