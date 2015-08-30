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

  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!is.element(outcome, outcomes)) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  outcome.only <- outcome.tidy[, c("Hospital.Name", "State", eval(outcome))]

  names(outcome.only)[names(outcome.only) == eval(outcome)] <- "three"
    
  hosps.ranked <- outcome.only %>% arrange(State,three,Hospital.Name) %>% mutate(rank = dense_rank(State, three, Hospital.Name))
  
  if (num == "best") {
    num.used <- 1
  } else if (num == "worst") {
    num.used <- max(hosps.ranked$rank)
  } else
    num.used <- num
  
  requested <- hosps.ranked %>% filter(rank == num.used) %>% arrange(State, Hospital.Name)

  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}