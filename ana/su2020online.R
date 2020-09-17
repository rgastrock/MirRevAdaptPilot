source('ana/shared.R')

#This script deals with the mirror reversal data collected online (Summer 2020)

# pre-processing----
#Files from online experiment have the whole trial's information within each row.
#Filenames are also different (contains URPP code, experiment, date, time)

#Function helps to organize data within each cell, so that they are numeric and we can work with them.
convertCellToNumVector <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}

#Function to handle one participant. Outputs a df with relevant information across trials
handleOneFile <- function(filename) {
  
  # if the file can't be read, return empty list for now
  df <- NULL
  try(df <- read.csv(filename, stringsAsFactors = F), silent = TRUE)
  if (is.null(df)) {
    return(list())
  }
  
  # set up vectors for relevant data:
  trialno <- c()            #trialNum
  targetangle_deg <- c()
  mirror <-c()              #trialsType
  reachdeviation_deg <- c()
  taskno <- c()             #trialsNum
  
  # remove empty lines:
  df <- df[which(!is.na(df$trialsNum)),]
  
  # loop through all trials
  for (trialnum in c(1:dim(df)[1])) {
    
    x <- convertCellToNumVector(df$trialMouse.x[trialnum])
    y <- convertCellToNumVector(df$trialMouse.y[trialnum])
    s <- convertCellToNumVector(df$step[trialnum])
    m <- df$trialsType[trialnum]
    a <- df$targetangle_deg[trialnum]
    
    # remove stuff that is not step==2
    step2idx = which(s == 2)
    x <- x[step2idx]
    y <- y[step2idx]
    
    #plot(x,y,type='l',col='blue',xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
    #lines(c(0,1),c(0,0),col='black')
    #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
    
    # get first point beyond some distance (home-target is 40% of height of participant's screen)
    # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
    d <- sqrt(x^2 + y^2)
    idx <- which(d > .12)[1]
    x <- x[idx]
    y <- y[idx]
    
    #points(x,y,col='red')
    
    # get angular deviation of reach from target angle:
    rotcoords <- rotateTrajectory(x,y,-a)
    x <- rotcoords[1]
    y <- rotcoords[2]
    
    rd <- (atan2(y, x) / pi) * 180
    
    
    #text(0,-0.1,sprintf('%0.3f',rd))
    
    # store in vectors:
    trialno <- c(trialno, trialnum)
    targetangle_deg <- c(targetangle_deg, a)
    mirror <-c(mirror, m)
    reachdeviation_deg <- c(reachdeviation_deg, rd)
    taskno <- c(taskno, df$trialsNum[trialnum])
    
  }
  
  # vectors as data frame columns:
  dfrd <- data.frame(trialno, targetangle_deg, mirror, reachdeviation_deg, taskno)
  
  # tasklist <- list()
  # 
  # for (taskno in c(1,2,3)) {
  #   
  #   taskdf <- dfrd[which(dfrd$taskno == taskno),]
  #   if (dim(taskdf)[1] != 160) { next }
  #   
  #   numrots <- length(unique(taskdf$mirror))
  #   condition <- list('4'='abrupt','7'='stepped','51'='gradual')[[sprintf('%d',numrots)]]
  #   modifier <- sign(taskdf$mirror[120])
  #   
  #   taskdf$mirror <- taskdf$mirror * modifier
  #   taskdf$reachdeviation_deg <- taskdf$reachdeviation_deg * modifier
  #   
  #   taskdf$reachdeviation_deg[which(abs(taskdf$reachdeviation_deg) > 60)] <- NA
  #   taskdf$reachdeviation_deg <- taskdf$reachdeviation_deg - mean(taskdf$reachdeviation_deg[17:32], na.rm=T)
  #   
  #   tasklist[[condition]] <- taskdf
  #   
  # }
  # 
  # # output:
  # return(tasklist)
  return(dfrd)
}



























# getNewFilenames <- function(){
#   old_filenames <- list.files("data/mReversalNewAlpha3-master/data_orig", pattern = "*.csv", full.names = TRUE)
#   
#   new_filenames <- c()
#   for(ppno in 1:length(old_filenames)){
#     dir.create(paste0("data/mReversalNewAlpha3-master/data/p",ppno))
#     ppfile <- sprintf("data/mReversalNewAlpha3-master/data/p%s/p%s.csv", ppno, ppno)
#     new_filenames <- c(new_filenames, ppfile)
#   }
# 
#   file.copy(from = old_filenames, to = new_filenames)
#   
#   #old filenames are in participant column of csv (in case it is needed).
#   #But we can also just compare it below:
#   #ndat <- cbind(old_filenames, new_filenames)
#   #return(ndat)
# }
# 
# getParticipantFullData <- function(maxppid = 131){
#   
#   for(ppno in 1:maxppid){
#     #print(ppno)
#     dat <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/p%s/p%s.csv', ppno, ppno), stringsAsFactors = FALSE)
#     #each cell has multiple values, so we want to separate them into their own rows
#     ndat <- separate_rows(dat, step, trialMouse.x, trialMouse.y, 
#                           trialMouse.leftButton, trialMouse.midButton, 
#                           trialMouse.rightButton, trialMouse.time, sep = ",", convert = TRUE)
#     #then some values would have brackets [ or ], so we would want to remove them
#     #ndat <- as.data.frame(lapply(ndat, function(y) {as.numeric(gsub("\\[|\\]", "", y))}))
#     ndat$step <- as.numeric(gsub("\\[|\\]", "", ndat$step))
#     ndat$trialMouse.x <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.x))
#     ndat$trialMouse.y <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.y))
#     ndat$trialMouse.leftButton <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.leftButton))
#     ndat$trialMouse.midButton <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.midButton))
#     ndat$trialMouse.rightButton <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.rightButton))
#     ndat$trialMouse.time <- as.numeric(gsub("\\[|\\]", "", ndat$trialMouse.time))
#     
#     
#     write.csv(ndat, file=sprintf('data/mReversalNewAlpha3-master/data/p%s/p%s_full.csv', ppno, ppno), row.names = F) 
#   }
# }