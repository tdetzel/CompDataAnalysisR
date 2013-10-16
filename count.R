count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) {
    stop("Cause is NULL!")
  }
  
  ## Check that specific "cause" is allowed; else throw error
  causes<-c("asphyxiation", "blunt force", "other",
            "shooting", "stabbing", "unknown")
  if (!(tolower(cause) %in% causes)) {
    stop("Cause does not exist!")
  }
  causeidx<-match(tolower(cause), causes)
  regexcauses<-c("[Aa]sphyxiation", "[Bb]lunt [Ff]orce", "[Oo]ther",
                 "[Ss]hooting", "[Ss]tabbing", "[Uu]nknown")
  
  ## Read "homicides.txt" data file
  homicides<-readLines("homicides.txt")
  
  ## Extract causes of death
  countcause<-0
  for (homicide in homicides) {
    rightcause<-regexec(paste("[Cc]ause: ", regexcauses[causeidx], sep=""), homicide)
    if (rightcause>0) {
      countcause<-countcause+1
    }
  }
  
  ## Return integer containing count of homicides for that cause
  countcause
}
