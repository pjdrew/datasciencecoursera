rankhospital <- function(state, outcome, num = "best") {
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
  
  if (!is.element(state, states)) {
    stop("invalid state")
  }
  
  if (!is.element(outcome, outcomes)) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  by.state <-
    outcome.tidy %>% filter(State == state)
  
  outcome.only <- by.state[, c("Hospital.Name", eval(outcome))]
  
  comp.outc <- outcome.only[complete.cases(outcome.only), ]
  
  names(comp.outc)[names(comp.outc) == eval(outcome)] <- "two"
  
  hosps.ranked <- comp.outc %>% arrange(two,Hospital.Name) %>% mutate(rank = order(two, Hospital.Name))
  
  if (num == "best") {
    num.used <- 1
  } else if (num == "worst") {
    num.used <- max(hosps.ranked$rank)
  } else
    num.used <- num

  requested <- hosps.ranked %>% filter(rank == num.used) %>% arrange(Hospital.Name)
  
  as.character(requested[1, 1])
  }