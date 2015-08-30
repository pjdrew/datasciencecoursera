rankall <- function(outcome, num = "best") {
  library(dplyr)
  ## Read outcome data
  outcome.data <-
    read.csv(
      "outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE,  na.strings = "Not Available"
    )[, c(2,7,11,17,23)]
  
  outcome.tidy <- outcome.data %>% select(
    Hospital.Name, State,
    'heart attack' = suppressWarnings(
      as.numeric(
        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
      )
    ),
    'heart failure' = suppressWarnings(
      as.numeric(
        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
      )
    ),
    'pneumonia' = suppressWarnings(
      as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    )
  )
  
  ## Check that state and outcome are valid
  states <- unique(outcome.tidy$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!is.element(outcome, outcomes)) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  outcome.only <- outcome.tidy[, c("Hospital.Name", "State", eval(outcome))]

  names(outcome.only)[names(outcome.only) == eval(outcome)] <- "three"
    
  hosps.ranked <- outcome.only %>% arrange(State,three,Hospital.Name) 
  
#   if (num == "best") {
#     num.used <- 1
#   } else if (num == "worst") {
#     num.used <- max(hosps.ranked$rank)
#   } else
#     num.used <- num
  
  hosps.subsetted <- data_frame(hospital=character(),state=character(), three=integer(), rank=double())
  
  for (st in 1:length(states))
  { 
    one.state <- hosps.ranked %>% filter(State == states[st]) %>% mutate(rank = order(State, three, Hospital.Name))
    
    if (num == "best") {
      num.used <- 1
    } else if (num == "worst") {
      num.used <- max(one.state$rank)
    } else
      num.used <- num
    
    if (num.used > max(one.state$rank)) {
      hosps.subsetted[nrow(hosps.subsetted) +1, ] <- c(NA, one.state$State, NA, NA)
    } else {
    hosps.subsetted[nrow(hosps.subsetted) +1, ] <- one.state %>% filter(rank == num.used) %>% arrange(Hospital.Name)}
  }
  
  requested <- hosps.subsetted %>% select(hospital, state) %>% arrange(state)

  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  requested
  }