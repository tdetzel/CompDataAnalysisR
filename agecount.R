agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if (is.null(age)) {
    stop("Age is NULL!")
  }
  
  ## Read "homicides.txt" data file
  homicides<-readLines("homicides.txt")
  
  ## Extract ages of victims; ignore records where no age is
  ## given
  ages<-c()
  indices<-grep("[0-9]+ years old", homicides)
  for (index in indices) {
    regex.result<-regexec("[0-9]+ years old", homicides[index])
    if (regex.result>0) {
      start<-regex.result[[1]][1]
      length<-attr(regex.result[[1]], "match.length")
      readage<-as.numeric(substr(homicides[index], start, start+length-10-1))
      ages<-append(ages, readage)
    }
  }
  
  ## Return integer containing count of homicides for that age
  length(subset(ages, ages==age))
}
