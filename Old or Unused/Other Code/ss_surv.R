ss.surv <- function( n.events, event.rate, censor.rate ) {
  ## Given the number of events calculated from a time-to-event sample size calculation assuming no censoring,
  ## this function computes the total sample size needed given the presence of censoring
  ## The key parameters are the event rate 'event.rate' and the censoring rate 'censor.rate'
  ## event.rate = Rate of events per unit time
  ## censor.rate = Rate of censoring per unit time
  inflation.factor <- ( event.rate + censor.rate ) / event.rate
  
  return( n.events * inflation.factor )
}
