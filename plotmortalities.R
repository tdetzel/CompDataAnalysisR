plotmartalities<-function() {
  outcome<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  par(mfrow=c(3,1))
  
  heartattack.mortalities<-outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[!is.na(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]
  heartattack.median<-median(heartattack.mortalities)
  heartattack.mean<-mean(heartattack.mortalities)
  hist(heartattack.mortalities, main=bquote("Heart Attack "*(tilde("X")==.(heartattack.mean))), xlim=range(5, 20), xlab="30-day Death Rate")
  abline(v=heartattack.median)
  
  heartfailure.mortalities<-outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[!is.na(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)]
  heartfailure.median<-median(heartfailure.mortalities)
  heartfailure.mean<-mean(heartfailure.mortalities)
  hist(heartfailure.mortalities, main=bquote("Heart Failure "*(tilde("X")==.(heartfailure.mean))), xlim=range(5, 20), xlab="30-day Death Rate")
  abline(v=heartfailure.median)
  
  pneumonia.mortalities<-outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[!is.na(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)]
  pneumonia.median<-median(pneumonia.mortalities)
  pneumonia.mean<-mean(pneumonia.mortalities)  
  hist(pneumonia.mortalities, main=bquote("Pneumonia "*(tilde("X")==.(pneumonia.mean))), xlim=range(5, 20), xlab="30-day Death Rate")
  abline(v=pneumonia.median)
}
