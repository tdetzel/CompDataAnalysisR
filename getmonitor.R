getmonitor <- function(id, directory, summarize = FALSE) {
  idStr<-formatC(id, digits=2,flag="000",format="d")
  filename<-paste(idStr, "csv", sep=".")
  filepath<-paste(directory, filename, sep="/")
  data<-read.csv(filepath)
  if (summarize) {
    print(summary(data))
  }
  data
}
