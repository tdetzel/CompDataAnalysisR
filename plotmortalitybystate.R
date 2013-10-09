plotmortalitybystate<-function() {
  outcome<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  statetable<-table(outcome$State)
  outcome<-subset(outcome, !is.na(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  outcome2<-subset(outcome, statetable[outcome$State]>20)
  death<-outcome2[,11]
  state<-outcome2$State
  par(las=2)
  statebymedian<-reorder(state, death, median)
  boxplot(death~statebymedian, ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by State")
}
