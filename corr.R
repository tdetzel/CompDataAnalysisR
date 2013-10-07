corr <- function(directory, threshold = 0) {
  correlations<-c()
  for (filename in dir(directory)) {
    filepath<-paste(directory, "/", filename, sep="")
    data<-read.csv(filepath)
    filtered.data<-subset(data, !is.na(data$sulfate) & !is.na(data$nitrate))
    if (nrow(filtered.data)>threshold) {
      correlations<-append(correlations, cor(filtered.data$sulfate, filtered.data$nitrate))
    }
  }
  correlations
}
