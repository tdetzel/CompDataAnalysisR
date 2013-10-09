library(lattice)

outcome<-read.csv("outcome-of-care-measures.csv", colClasses="character")
hospital<-read.csv("hospital-data.csv", colClasses="character")
outcome.hospital<-merge(outcome, hospital, by="Provider.Number")
death<-as.numeric(outcome.hospital[,11])
npatient<-as.numeric(outcome.hospital[,15])
owner<-factor(outcome.hospital$Hospital.Ownership)

xyplot(death~npatient | owner,
       main="Heart Attach 30-day Death Rate by Ownership",
       xlab="Number of Patients Seen", 
       ylab="30-day Death Rate", 
       panel=function(x, y, ...) {
         panel.xyplot(x, y, ...)
         fit<-lm(y~x)
         panel.abline(fit)
       })
