source('ana/shared.R')

#This script deals with the mirror reversal data collected online (Summer 2020)

# pre-processing----
#First, we need to rename the files and organize them similarly to the tablet data.
#This will help to use the scripts in the tablet data to analyze the online data.
#Some files are duplicated and/or removed due to technical difficulties. These are tracked in a separate document.

getNewFilenames <- function(){
  old_filenames <- list.files("data/mReversalNewAlpha3-master/data_orig", pattern = "*.csv", full.names = TRUE)
  
  new_filenames <- c()
  for(ppno in 1:length(old_filenames)){
    dir.create(paste0("data/mReversalNewAlpha3-master/data/p",ppno))
    ppfile <- sprintf("data/mReversalNewAlpha3-master/data/p%s/p%s.csv", ppno, ppno)
    new_filenames <- c(new_filenames, ppfile)
  }

  file.copy(from = old_filenames, to = new_filenames)
  
  #old filenames are in participant column of csv (in case it is needed).
  #But we can also just compare it below:
  #ndat <- cbind(old_filenames, new_filenames)
  #return(ndat)
}

getParticipantFullData <- function(maxppid = 131){
  
  for(ppno in 1:maxppid){
    #print(ppno)
    dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/p%s/p%s.csv', ppno, ppno), stringsAsFactors = FALSE)
    #each cell has multiple values, so we want to separate them into their own rows
    ndat <- separate_rows(dat, step, trialMouse.x, trialMouse.y, 
                          trialMouse.leftButton, trialMouse.midButton, 
                          trialMouse.rightButton, trialMouse.time, sep = ",", convert = TRUE)
    #then some values would have brackets [ or ], so we would want to remove them
    #ndat <- as.data.frame(lapply(ndat, function(y) {as.numeric(gsub("\\[|\\]", "", y))}))
    ndat$step <- as.numeric(gsub("\\[|\\]", "", ndat$step))
    ndat$trialMouse.x <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.x))
    ndat$trialMouse.y <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.y))
    ndat$trialMouse.leftButton <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.leftButton))
    ndat$trialMouse.midButton <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.midButton))
    ndat$trialMouse.rightButton <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.rightButton))
    ndat$trialMouse.time <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.time))
    
    
    write.csv(ndat, file=sprintf('data/mReversalNewAlpha3-master/data/p%s/p%s_full.csv', ppno, ppno), row.names = F) 
  }
}