complete <- function(directory, id = 1:332) {
  nobs<-c()
  for (i in id) {
    strID<-formatC(i, digit=2, flag="000", format="d")
    filepath<-paste(directory, "/", strID, ".csv", sep="")
    data<-read.csv(filepath)
    ndata<-nrow(subset(data, !is.na(data$sulfate) & !is.na(data$nitrate)))
    nobs<-append(nobs, ndata)
  }
  data.frame(id=id, nobs=nobs)
}
