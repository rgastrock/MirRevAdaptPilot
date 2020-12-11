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
  participant <- c()
  
  # remove empty lines:
  df <- df[which(!is.na(df$trialsNum)),]
  #df <- df[which(df$trialsNum == 2),]
  
  # loop through all trials
  #plot(x,y,type='l',col='blue',xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
  for (trialnum in c(1:dim(df)[1])) {
    
    x <- convertCellToNumVector(df$trialMouse.x[trialnum])
    y <- convertCellToNumVector(df$trialMouse.y[trialnum])
    s <- convertCellToNumVector(df$step[trialnum])
    m <- df$trialsType[trialnum]
    a <- df$targetangle_deg[trialnum]
    p <- df$participant[trialnum]
    
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
    idx <- which(d > .08)[1]
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
    participant <- c(participant, p)
  }
  
  # vectors as data frame columns:
  dfrd <- data.frame(trialno, targetangle_deg, mirror, reachdeviation_deg, taskno, participant)
  
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


#test initial trials ----
getParticipantTrialData<- function(group, set) {

  data <- getGroupCircularLC(group=group, set=set)
  #get only participants with data for this group
  ndat <- data[1,]
  #get only participants with reaches in "correct" side of mirror
  ndat[which(abs(ndat) < 90)] <- NA
  #get only those with data
  ndat <- ndat[,which(!is.na(ndat))]
  #ndat <- ndat[,2:ncol(ndat)]
  
  #can return reachdevs for these participants
  #return(ndat)
  #or just return participant names
  ppnames <- colnames(ndat)
  return(ppnames)
}


getParticipantFilenames<- function(group,set) {
  #get filenames of participants reaching in correct side, so that we can plot their trajectories
  ppnames <- getParticipantTrialData(group=group, set=set)
  
  datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  
  filenames <- c()
  for(participant in ppnames){
    datafilename <- grep(participant, datafilenames, value = TRUE)
    
    filenames <- c(filenames, datafilename)
  }
  return(filenames)
}  
  
plotReachTrajectories <- function(group,set){

  #par(mfrow = c(5,5))
  datafilenames <- getParticipantFilenames(group=group, set=set) #will only contain those part of group and those reaching to correct side
  triali <- c(21)

  # plot(NA,NA,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), xlab = 'X coords', ylab = 'Y coords', main = sprintf('Mir Step2, Trial %d', triali))
  # points(c(0,.4*(cos((30/180)*pi))),c(0,.4*(sin((30/180)*pi))),col='black')
  # points(c(0,.4*(cos((60/180)*pi))),c(0,.4*(sin((60/180)*pi))),col='black')
  #cat(sprintf('%d\n', triali))
  #data <- c()
  
  pdf(sprintf("data/mirrorreversal-fall/doc/fig/Trial1Trajectories_%sTargetLoc.pdf", group))
  
  for(datafilenum in c(1:length(datafilenames))){
    datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory

    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    try(df <- read.csv(datafilename, stringsAsFactors = F), silent = TRUE)
    
    #plot
    pp <- unique(df$participant)
    plot(NA,NA,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), xlab = 'X coords', ylab = 'Y coords', main = sprintf('%s-deg Target, Mirror Trial 1, ID:%s', group,pp))
    points(c(0,.4*(cos((30/180)*pi))),c(0,.4*(sin((30/180)*pi))),col='black')
    points(c(0,.4*(cos((60/180)*pi))),c(0,.4*(sin((60/180)*pi))),col='black')
    # set up vectors for relevant data:
    trialno <- c()            #trialNum
    targetangle_deg <- c()
    mirror <-c()              #trialsType
    reachdeviation_deg <- c()
    taskno <- c()             #trialsNum
    participant <- c()

    # remove empty lines:
    df <- df[which(!is.na(df$trialsNum)),]
    df <- df[which(df$trialNum == triali),]

    # loop through all trials



    x <- convertCellToNumVector(df$trialMouse.x)
    y <- convertCellToNumVector(df$trialMouse.y)
    s <- convertCellToNumVector(df$step)
    m <- df$trialsType
    a <- df$targetangle_deg
    p <- df$participant

    # remove stuff that is not step==1
    step2idx = which(s == 2)
    x2 <- x[step2idx]
    y2 <- y[step2idx]

    lines(x2,y2,type='l',col=alpha('blue', 1),xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
    lines(c(0,1),c(0,0),col='black')
    #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
    
    # remove stuff that is not step==1
    step1idx = which(s == 1)
    x1 <- x[step1idx]
    y1 <- y[step1idx]
    
    lines(x1,y1,type='l',col=alpha('grey', 1),xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
    
    # get first point beyond some distance (home-target is 40% of height of participant's screen)
    # we can set a cutoff at 30% of home-target distance (20% of .4 = .08)
    d <- sqrt(x2^2 + y2^2)
    idx <- which(d > .08)[1]
    x2 <- x2[idx]
    y2 <- y2[idx]

    points(x2,y2,col='red')


    # get angular deviation of reach from target angle:
    # rotcoords <- rotateTrajectory(x,y,-a)
    # x <- rotcoords[1]
    # y <- rotcoords[2]
    # 
    # rd <- (atan2(y, x) / pi) * 180


    #text(0,-0.1,sprintf('%0.3f',rd))
  }
  dev.off()
} 
  
  
  
  
  



#plot trajectories for step 1-----
plotTestOneFileStepOne <- function() {
  
  par(mfrow = c(3,4))
  datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  triallist <- c(21:30)
  for (triali in triallist){
    plot(NA,NA,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), xlab = 'X coords', ylab = 'Y coords', main = sprintf('Mir Step 1, Trial %d', triali))
    points(c(0,.4*(cos((30/180)*pi))),c(0,.4*(sin((30/180)*pi))),col='black')
    points(c(0,.4*(cos((60/180)*pi))),c(0,.4*(sin((60/180)*pi))),col='black')
    cat(sprintf('%d\n', triali))
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/mirrorreversal-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      try(df <- read.csv(datafilename, stringsAsFactors = F), silent = TRUE)
      
      # set up vectors for relevant data:
      trialno <- c()            #trialNum
      targetangle_deg <- c()
      mirror <-c()              #trialsType
      reachdeviation_deg <- c()
      taskno <- c()             #trialsNum
      participant <- c()
      
      # remove empty lines:
      df <- df[which(!is.na(df$trialsNum)),]
      df <- df[which(df$trialNum == triali),]
      
      x <- convertCellToNumVector(df$trialMouse.x)
      y <- convertCellToNumVector(df$trialMouse.y)
      s <- convertCellToNumVector(df$step)
      m <- df$trialsType
      a <- df$targetangle_deg
      p <- df$participant
      
      # remove stuff that is not step==1
      step1idx = which(s == 1)
      x <- x[step1idx]
      y <- y[step1idx]
      
      #points(x,y,col='blue',xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
      lines(x,y,type='l', col=alpha('blue', 0.2),xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
      lines(c(0,1),c(0,0),col='black')
      #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
      
      # get first point beyond some distance (home-target is 40% of height of participant's screen)
      # we can set a cutoff at 30% of home-target distance (30% of .4 = .12; 20% of .4 = .08)
      # d <- sqrt(x^2 + y^2)
      # idx <- which(d > .08)[1]
      # x <- x[idx]
      # y <- y[idx]
      # 
      # points(x,y,col='red')
      
      
      # get angular deviation of reach from target angle:
      # rotcoords <- rotateTrajectory(x,y,-a)
      # x <- rotcoords[1]
      # y <- rotcoords[2]
      # 
      # rd <- (atan2(y, x) / pi) * 180
      
      
      #text(0,-0.1,sprintf('%0.3f',rd))
    }
  }
  
  
}

plotAlOneFileStepOne <- function() {
  
  par(mfrow = c(3,4))
  datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  triallist <- c(1:10)
  for (triali in triallist){
    plot(NA,NA,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), xlab = 'X coords', ylab = 'Y coords', main = sprintf('Aligned Step 1, Trial %d', triali))
    points(c(0,.4*(cos((30/180)*pi))),c(0,.4*(sin((30/180)*pi))),col='black')
    points(c(0,.4*(cos((60/180)*pi))),c(0,.4*(sin((60/180)*pi))),col='black')
    cat(sprintf('%d\n', triali))
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      try(df <- read.csv(datafilename, stringsAsFactors = F), silent = TRUE)
      
      # set up vectors for relevant data:
      trialno <- c()            #trialNum
      targetangle_deg <- c()
      mirror <-c()              #trialsType
      reachdeviation_deg <- c()
      taskno <- c()             #trialsNum
      participant <- c()
      
      # remove empty lines:
      df <- df[which(!is.na(df$trialsNum)),]
      df <- df[which(df$trialNum == triali),]
      
      x <- convertCellToNumVector(df$trialMouse.x)
      y <- convertCellToNumVector(df$trialMouse.y)
      s <- convertCellToNumVector(df$step)
      m <- df$trialsType
      a <- df$targetangle_deg
      p <- df$participant
      
      # remove stuff that is not step==1
      step1idx = which(s == 1)
      x <- x[step1idx]
      y <- y[step1idx]
      
      lines(x,y,type='l',col=alpha('blue', 0.2),xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
      lines(c(0,1),c(0,0),col='black')
      #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
      
      # get first point beyond some distance (home-target is 40% of height of participant's screen)
      # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
      # d <- sqrt(x^2 + y^2)
      # idx <- which(d > .08)[1]
      # x <- x[idx]
      # y <- y[idx]
      # 
      # points(x,y,col='red')
      
      
      # get angular deviation of reach from target angle:
      # rotcoords <- rotateTrajectory(x,y,-a)
      # x <- rotcoords[1]
      # y <- rotcoords[2]
      # 
      # rd <- (atan2(y, x) / pi) * 180
      
      
      #text(0,-0.1,sprintf('%0.3f',rd))
    }
  }
  
  
}

plotMirOneFileStepOne <- function() {
  
  par(mfrow = c(3,4))
  datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  triallist <- c(21:30)
  for (triali in triallist){
    plot(NA,NA,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), xlab = 'X coords', ylab = 'Y coords', main = sprintf('Mir Step1, Trial %d', triali))
    points(c(0,.4*(cos((30/180)*pi))),c(0,.4*(sin((30/180)*pi))),col='black')
    points(c(0,.4*(cos((60/180)*pi))),c(0,.4*(sin((60/180)*pi))),col='black')
    cat(sprintf('%d\n', triali))
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      try(df <- read.csv(datafilename, stringsAsFactors = F), silent = TRUE)
      
      # set up vectors for relevant data:
      trialno <- c()            #trialNum
      targetangle_deg <- c()
      mirror <-c()              #trialsType
      reachdeviation_deg <- c()
      taskno <- c()             #trialsNum
      participant <- c()
      
      # remove empty lines:
      df <- df[which(!is.na(df$trialsNum)),]
      df <- df[which(df$trialNum == triali),]
      
      # loop through all trials
      
      
      
      x <- convertCellToNumVector(df$trialMouse.x)
      y <- convertCellToNumVector(df$trialMouse.y)
      s <- convertCellToNumVector(df$step)
      m <- df$trialsType
      a <- df$targetangle_deg
      p <- df$participant
      
      # remove stuff that is not step==1
      step1idx = which(s == 1)
      x <- x[step1idx]
      y <- y[step1idx]
      
      lines(x,y,type='l',col=alpha('blue', 0.2),xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
      lines(c(0,1),c(0,0),col='black')
      #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
      
      # get first point beyond some distance (home-target is 40% of height of participant's screen)
      # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
      # d <- sqrt(x^2 + y^2)
      # idx <- which(d > .12)[1]
      # x <- x[idx]
      # y <- y[idx]
      # 
      # points(x,y,col='red')
      
      
      # get angular deviation of reach from target angle:
      # rotcoords <- rotateTrajectory(x,y,-a)
      # x <- rotcoords[1]
      # y <- rotcoords[2]
      # 
      # rd <- (atan2(y, x) / pi) * 180
      
      
      #text(0,-0.1,sprintf('%0.3f',rd))
    }
  }
  
  
}


# plot trajectories for step 2-----
plotTestOneFileStepTwo <- function() {
  
  par(mfrow = c(3,4))
  datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  triallist <- c(21:30)
  for (triali in triallist){
    plot(NA,NA,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), xlab = 'X coords', ylab = 'Y coords', main = sprintf('Mir Step 2, Trial %d', triali))
    points(c(0,.4*(cos((30/180)*pi))),c(0,.4*(sin((30/180)*pi))),col='black')
    points(c(0,.4*(cos((60/180)*pi))),c(0,.4*(sin((60/180)*pi))),col='black')
    cat(sprintf('%d\n', triali))
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/mirrorreversal-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      try(df <- read.csv(datafilename, stringsAsFactors = F), silent = TRUE)
      
      # set up vectors for relevant data:
      trialno <- c()            #trialNum
      targetangle_deg <- c()
      mirror <-c()              #trialsType
      reachdeviation_deg <- c()
      taskno <- c()             #trialsNum
      participant <- c()
      
      # remove empty lines:
      df <- df[which(!is.na(df$trialsNum)),]
      df <- df[which(df$trialNum == triali),]
      
      # loop through all trials
      
      
      
      x <- convertCellToNumVector(df$trialMouse.x)
      y <- convertCellToNumVector(df$trialMouse.y)
      s <- convertCellToNumVector(df$step)
      m <- df$trialsType
      a <- df$targetangle_deg
      p <- df$participant
      
      # remove stuff that is not step==2
      step2idx = which(s == 2)
      x <- x[step2idx]
      y <- y[step2idx]
      
      lines(x,y,type='l',col=alpha('blue', 0.1),xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
      lines(c(0,1),c(0,0),col='black')
      #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
      
      # get first point beyond some distance (home-target is 40% of height of participant's screen)
      # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
      # d <- sqrt(x^2 + y^2)
      # idx <- which(d > .12)[1]
      # x <- x[idx]
      # y <- y[idx]
      # 
      # points(x,y,col='red')
      
      
      # get angular deviation of reach from target angle:
      # rotcoords <- rotateTrajectory(x,y,-a)
      # x <- rotcoords[1]
      # y <- rotcoords[2]
      # 
      # rd <- (atan2(y, x) / pi) * 180
      
      
      #text(0,-0.1,sprintf('%0.3f',rd))
    }
  }
  
  
}

plotAlOneFileStepTwo <- function() {
  
  par(mfrow = c(3,4))
  datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  triallist <- c(1:10)
  for (triali in triallist){
    plot(NA,NA,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), xlab = 'X coords', ylab = 'Y coords', main = sprintf('Aligned, Step 2, Trial %d', triali))
    points(c(0,.4*(cos((30/180)*pi))),c(0,.4*(sin((30/180)*pi))),col='black')
    points(c(0,.4*(cos((60/180)*pi))),c(0,.4*(sin((60/180)*pi))),col='black')
    cat(sprintf('%d\n', triali))
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      try(df <- read.csv(datafilename, stringsAsFactors = F), silent = TRUE)
      
      # set up vectors for relevant data:
      trialno <- c()            #trialNum
      targetangle_deg <- c()
      mirror <-c()              #trialsType
      reachdeviation_deg <- c()
      taskno <- c()             #trialsNum
      participant <- c()
      
      # remove empty lines:
      df <- df[which(!is.na(df$trialsNum)),]
      df <- df[which(df$trialNum == triali),]
      
      # loop through all trials
      
      
      
      x <- convertCellToNumVector(df$trialMouse.x)
      y <- convertCellToNumVector(df$trialMouse.y)
      s <- convertCellToNumVector(df$step)
      m <- df$trialsType
      a <- df$targetangle_deg
      p <- df$participant
      
      # remove stuff that is not step==2
      step2idx = which(s == 2)
      x <- x[step2idx]
      y <- y[step2idx]
      
      lines(x,y,type='l',col=alpha('blue', 0.1),xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
      lines(c(0,1),c(0,0),col='black')
      #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
      
      # get first point beyond some distance (home-target is 40% of height of participant's screen)
      # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
      d <- sqrt(x^2 + y^2)
      idx <- which(d > .12)[1]
      x <- x[idx]
      y <- y[idx]

      points(x,y,col='red')
      
      
      # get angular deviation of reach from target angle:
      # rotcoords <- rotateTrajectory(x,y,-a)
      # x <- rotcoords[1]
      # y <- rotcoords[2]
      # 
      # rd <- (atan2(y, x) / pi) * 180
      
      
      #text(0,-0.1,sprintf('%0.3f',rd))
    }
  }
  
  
}

plotMirOneFileStepTwo <- function() {
  
  par(mfrow = c(3,4))
  datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  triallist <- c(21:30)
  for (triali in triallist){
    plot(NA,NA,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), xlab = 'X coords', ylab = 'Y coords', main = sprintf('Mir Step 2, Trial %d', triali))
    points(c(0,.4*(cos((30/180)*pi))),c(0,.4*(sin((30/180)*pi))),col='black')
    points(c(0,.4*(cos((60/180)*pi))),c(0,.4*(sin((60/180)*pi))),col='black')
    cat(sprintf('%d\n', triali))
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      try(df <- read.csv(datafilename, stringsAsFactors = F), silent = TRUE)
      
      # set up vectors for relevant data:
      trialno <- c()            #trialNum
      targetangle_deg <- c()
      mirror <-c()              #trialsType
      reachdeviation_deg <- c()
      taskno <- c()             #trialsNum
      participant <- c()
      
      # remove empty lines:
      df <- df[which(!is.na(df$trialsNum)),]
      df <- df[which(df$trialNum == triali),]
      
      # loop through all trials
      
      
      
      x <- convertCellToNumVector(df$trialMouse.x)
      y <- convertCellToNumVector(df$trialMouse.y)
      s <- convertCellToNumVector(df$step)
      m <- df$trialsType
      a <- df$targetangle_deg
      p <- df$participant
      
      # remove stuff that is not step==2
      step2idx = which(s == 2)
      x <- x[step2idx]
      y <- y[step2idx]
      
      lines(x,y,type='l',col=alpha('blue', 0.1),xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
      lines(c(0,1),c(0,0),col='black')
      #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
      
      # get first point beyond some distance (home-target is 40% of height of participant's screen)
      # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
      d <- sqrt(x^2 + y^2)
      idx <- which(d > .12)[1]
      x <- x[idx]
      y <- y[idx]
      
      points(x,y,col='red')
      
      
      # get angular deviation of reach from target angle:
      # rotcoords <- rotateTrajectory(x,y,-a)
      # x <- rotcoords[1]
      # y <- rotcoords[2]
      # 
      # rd <- (atan2(y, x) / pi) * 180
      
      
      #text(0,-0.1,sprintf('%0.3f',rd))
    }
  }
  
  
}

# plot trajectories for step 3-----
plotTestOneFileStepThree <- function() {
  
  par(mfrow = c(3,4))
  datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  triallist <- c(21:30)
  for (triali in triallist){
    plot(NA,NA,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), xlab = 'X coords', ylab = 'Y coords', main = sprintf('Mir Step 3, Trial %d', triali))
    points(c(0,.4*(cos((30/180)*pi))),c(0,.4*(sin((30/180)*pi))),col='black')
    points(c(0,.4*(cos((60/180)*pi))),c(0,.4*(sin((60/180)*pi))),col='black')
    cat(sprintf('%d\n', triali))
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/mirrorreversal-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      try(df <- read.csv(datafilename, stringsAsFactors = F), silent = TRUE)
      
      # set up vectors for relevant data:
      trialno <- c()            #trialNum
      targetangle_deg <- c()
      mirror <-c()              #trialsType
      reachdeviation_deg <- c()
      taskno <- c()             #trialsNum
      participant <- c()
      
      # remove empty lines:
      df <- df[which(!is.na(df$trialsNum)),]
      df <- df[which(df$trialNum == triali),]
      
      # loop through all trials
      
      
      
      x <- convertCellToNumVector(df$trialMouse.x)
      y <- convertCellToNumVector(df$trialMouse.y)
      s <- convertCellToNumVector(df$step)
      m <- df$trialsType
      a <- df$targetangle_deg
      p <- df$participant
      
      # remove stuff that is not step==3
      step3idx = which(s == 3)
      x <- x[step3idx]
      y <- y[step3idx]
      
      lines(x,y,type='l',col=alpha('blue', 0.1),xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
      lines(c(0,1),c(0,0),col='black')
      #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
      
      # get first point beyond some distance (home-target is 40% of height of participant's screen)
      # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
      # d <- sqrt(x^2 + y^2)
      # idx <- which(d < .12)[1]
      # x <- x[idx]
      # y <- y[idx]
      # 
      # points(x,y,col='red')
      
      
      # get angular deviation of reach from target angle:
      # rotcoords <- rotateTrajectory(x,y,-a)
      # x <- rotcoords[1]
      # y <- rotcoords[2]
      # 
      # rd <- (atan2(y, x) / pi) * 180
      
      
      #text(0,-0.1,sprintf('%0.3f',rd))
    }
  }
  
  
}

plotAlOneFileStepThree <- function() {
  
  par(mfrow = c(3,4))
  datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  triallist <- c(1:10)
  for (triali in triallist){
    plot(NA,NA,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), xlab = 'X coords', ylab = 'Y coords', main = sprintf('Aligned, Step 3, Trial %d', triali))
    points(c(0,.4*(cos((30/180)*pi))),c(0,.4*(sin((30/180)*pi))),col='black')
    points(c(0,.4*(cos((60/180)*pi))),c(0,.4*(sin((60/180)*pi))),col='black')
    cat(sprintf('%d\n', triali))
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      try(df <- read.csv(datafilename, stringsAsFactors = F), silent = TRUE)
      
      # set up vectors for relevant data:
      trialno <- c()            #trialNum
      targetangle_deg <- c()
      mirror <-c()              #trialsType
      reachdeviation_deg <- c()
      taskno <- c()             #trialsNum
      participant <- c()
      
      # remove empty lines:
      df <- df[which(!is.na(df$trialsNum)),]
      df <- df[which(df$trialNum == triali),]
      
      # loop through all trials
      
      
      
      x <- convertCellToNumVector(df$trialMouse.x)
      y <- convertCellToNumVector(df$trialMouse.y)
      s <- convertCellToNumVector(df$step)
      m <- df$trialsType
      a <- df$targetangle_deg
      p <- df$participant
      
      # remove stuff that is not step==3
      step3idx = which(s == 3)
      x <- x[step3idx]
      y <- y[step3idx]
      
      lines(x,y,type='l',col=alpha('blue', 0.1),xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
      lines(c(0,1),c(0,0),col='black')
      #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
      
      # get first point beyond some distance (home-target is 40% of height of participant's screen)
      # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
      # d <- sqrt(x^2 + y^2)
      # idx <- which(d > .12)[1]
      # x <- x[idx]
      # y <- y[idx]
      # 
      # points(x,y,col='red')
      
      
      # get angular deviation of reach from target angle:
      # rotcoords <- rotateTrajectory(x,y,-a)
      # x <- rotcoords[1]
      # y <- rotcoords[2]
      # 
      # rd <- (atan2(y, x) / pi) * 180
      
      
      #text(0,-0.1,sprintf('%0.3f',rd))
    }
  }
  
  
}

plotMirOneFileStepThree <- function() {
  
  par(mfrow = c(3,4))
  datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  triallist <- c(21:30)
  for (triali in triallist){
    plot(NA,NA,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), xlab = 'X coords', ylab = 'Y coords', main = sprintf('Mir Step 3, Trial %d', triali))
    points(c(0,.4*(cos((30/180)*pi))),c(0,.4*(sin((30/180)*pi))),col='black')
    points(c(0,.4*(cos((60/180)*pi))),c(0,.4*(sin((60/180)*pi))),col='black')
    cat(sprintf('%d\n', triali))
    for(datafilenum in c(1:length(datafilenames))){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
      
      #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
      try(df <- read.csv(datafilename, stringsAsFactors = F), silent = TRUE)
      
      # set up vectors for relevant data:
      trialno <- c()            #trialNum
      targetangle_deg <- c()
      mirror <-c()              #trialsType
      reachdeviation_deg <- c()
      taskno <- c()             #trialsNum
      participant <- c()
      
      # remove empty lines:
      df <- df[which(!is.na(df$trialsNum)),]
      df <- df[which(df$trialNum == triali),]
      
      # loop through all trials
      
      
      
      x <- convertCellToNumVector(df$trialMouse.x)
      y <- convertCellToNumVector(df$trialMouse.y)
      s <- convertCellToNumVector(df$step)
      m <- df$trialsType
      a <- df$targetangle_deg
      p <- df$participant
      
      # remove stuff that is not step==3
      step3idx = which(s == 3)
      x <- x[step3idx]
      y <- y[step3idx]
      
      lines(x,y,type='l',col=alpha('blue', 0.1),xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
      lines(c(0,1),c(0,0),col='black')
      #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
      
      # get first point beyond some distance (home-target is 40% of height of participant's screen)
      # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
      # d <- sqrt(x^2 + y^2)
      # idx <- which(d > .12)[1]
      # x <- x[idx]
      # y <- y[idx]
      # 
      # points(x,y,col='red')
      
      
      # get angular deviation of reach from target angle:
      # rotcoords <- rotateTrajectory(x,y,-a)
      # x <- rotcoords[1]
      # y <- rotcoords[2]
      # 
      # rd <- (atan2(y, x) / pi) * 180
      
      
      #text(0,-0.1,sprintf('%0.3f',rd))
    }
  }
  
  
}


#density or frequency plots: CIRCULAR-----
# getParticipantCircFreq <- function(filename){
#   dat <- handleOneFile(filename = filename)
#   mdat <- dat[which(dat$taskno == 2), ]
#   
#   #transform reachdev to circular
#   mdat_angles <- as.numeric(mdat$reachdeviation_deg)
#   mdat$circ_reachdev <- as.circular(mdat_angles, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
# 
#   return(mdat)
# }

# getGroupCircFreq <- function(group, set){
#   
#   if (set == 'su2020'){
#     datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
#     #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
#   } else if (set == 'fa2020'){
#     datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
#   }
#   
#   dataoutput<- data.frame() #create place holder
#   for(datafilenum in c(1:length(datafilenames))){
#     if (set == 'su2020'){
#       datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
#     } else if (set == 'fa2020'){
#       datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
#     }
#     cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
#     mdat <- getParticipantCircFreq(filename = datafilename)
# 
#     # per target location, get reachdev for corresponding trials
#     
#     trial <- c(1:length(mdat$trialno))
#     mdat$trialno <- trial
#     for (triali in trial){
#       trialdat <- mdat[which(mdat$trialno == triali),]
#       #set reachdev to NA if not the target location we want
#       if (trialdat$targetangle_deg != group){
#         trialdat$reachdeviation_deg <- NA
#       }
#       mdat[triali,] <- trialdat
#     }
#     ppreaches <- mdat$reachdeviation_deg #get reach deviations column from learning curve data
#     ppdat <- data.frame(trial, ppreaches)
#     
#     ppname <- unique(mdat$participant)
#     names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
#     
#     if (prod(dim(dataoutput)) == 0){
#       dataoutput <- ppdat
#     } else {
#       dataoutput <- cbind(dataoutput, ppreaches)
#       names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
#     }
#   }
#   
#   
#   return(dataoutput)
#   
#   #removed outlier procedure first. Due to circular statistics, mean and sd now differ.
#   #typical outlier removal procedure would not be valid in this case
# }

plotGroupCircFreq <- function(groups = c('30', '60'), set){
  
  for(group in groups){
    #dat <- getGroupCircFreq(group = group, set = set)
    dat <- getGroupCircularLC(group=group, set=set)
    if(set == 'fa2020'){
      pdf(sprintf("data/mirrorreversal-fall/doc/fig/Distribution_%sCircular.pdf", group))
    } else if (set == 'su2020'){
      pdf(sprintf("data/mReversalNewAlpha3-master/doc/fig/Distribution_%sCircular.pdf", group))
    }
    
    #current fix for summer data being non-randomized and not counterbalanced
    #triallist <- dat$trial
    triallist <- c(1:90)
    
    if(group == '30' & set == 'su2020'){
      n <- triallist[seq(1,length(triallist),2)]
      dat <- dat[n,]
      triallist <- dat$trial
    } else if (group == '60' & set == 'su2020'){
      n <- triallist[seq(2,length(triallist),2)]
      dat <- dat[n,]
      triallist <- dat$trial
    }
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,2:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      plot(distsubdat, main = sprintf('%s° Target: Trial %s', group, triali), plot.type = 'circle', shrink=1.20)
      if(group == '30'){
        rd <- as.circular(c(0,120), type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        points.circular(rd, pch = 15, col = 'red')
        lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.20)
        #abline(v = 120, col = 8, lty = 2)
      } else if (group == '60'){
        rd <- as.circular(c(0,60), type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        points.circular(rd, pch = 15, col = 'red')
        lines(distsubdat, points.plot=TRUE, col=4, points.col=4, shrink=1.20)
        #abline(v = 60, col = 8, lty = 2)
      }
      # axis(1, at = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 330, 300, 360))
      # axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    }
    dev.off()
    
  }
}

#density plots according to step 1 samples----
#make changes to handling of file
handleOneFileCheck <- function(filename) {
  
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
  participant <- c()
  step1_samp <- c() #if there are multiple step 1 samples, then participant did not stay at home position
  
  # remove empty lines:
  df <- df[which(!is.na(df$trialsNum)),]
  #df <- df[which(df$trialsNum == 2),]
  
  # loop through all trials
  #plot(x,y,type='l',col='blue',xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
  for (trialnum in c(1:dim(df)[1])) {
    
    x <- convertCellToNumVector(df$trialMouse.x[trialnum])
    y <- convertCellToNumVector(df$trialMouse.y[trialnum])
    s <- convertCellToNumVector(df$step[trialnum])
    m <- df$trialsType[trialnum]
    a <- df$targetangle_deg[trialnum]
    p <- df$participant[trialnum]
    
    #check how many samples for step 1
    step1idx = which(s == 1)
    
    if (length(step1idx) > 2){
      s1 <- 1 #1 means it has more than 2 samples
    } else{
      s1 <- 0 #0 means it only has 2 samples
    }
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
    idx <- which(d > .08)[1]
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
    participant <- c(participant, p)
    step1_samp <- c(step1_samp, s1)
  }
  
  # vectors as data frame columns:
  dfrd <- data.frame(trialno, targetangle_deg, mirror, reachdeviation_deg, taskno, participant, step1_samp)
  
  
  return(dfrd)
}
#participant data will now be structured differently
getCheckParticipantCircularLC <- function(filename){
  
  #first, implement baseline correction
  #get Aligned biases
  dat <- handleOneFileCheck(filename = filename)
  dat$circ_rd <- as.circular(dat$reachdeviation_deg, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  
  adat <- dat[which(dat$taskno == 1), ]
  # use cleaned baseline data (reaches in correct quadrant)
  for (trialno in adat$trialno){
    #go through each trial, replace outlier values with NA
    subadat <- adat[trialno,]
    if (subadat$targetangle_deg == '30'){
      subadat$circ_rd[which(subadat$circ_rd < -30 | subadat$circ_rd > 60)] <- NA
    } else if (subadat$targetangle_deg == '60'){
      subadat$circ_rd[which(subadat$circ_rd < -60 | subadat$circ_rd > 30)] <- NA
    }
    adat[trialno, ] <- subadat
  }
  
  biases <- aggregate(circ_rd ~ targetangle_deg, data= adat, FUN = median.circular) 
  
  mdat <- dat[which(dat$taskno == 2),]
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
    bias<- biases[biasno, 'circ_rd'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    mdat$circ_rd[which(mdat$targetangle_deg == target)] <- mdat$circ_rd[which(mdat$targetangle_deg == target)] - bias
    
  }
  return(mdat)
}

#grouped data will be the reachdevs to complete plot AND step1_samp
  
getCheckGroupCircularLC <- function(group, set){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getCheckParticipantCircularLC(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(mdat$trialno))
    mdat$trialno <- trial
    for (triali in trial){
      trialdat <- mdat[which(mdat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$circ_rd <- NA
      }
      mdat[triali,] <- trialdat
    }
    ppreaches <- mdat$circ_rd #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, ppreaches)
    
    ppname <- unique(mdat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  
  return(dataoutput)
  
  #removed outlier procedure first. Due to circular statistics, mean and sd now differ.
  #typical outlier removal procedure would not be valid in this case
}

getCheckGroupStep1Samp <- function(group, set){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getCheckParticipantCircularLC(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(mdat$trialno))
    mdat$trialno <- trial
    for (triali in trial){
      trialdat <- mdat[which(mdat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$step1_samp <- NA
      }
      mdat[triali,] <- trialdat
    }
    ppreaches <- mdat$step1_samp #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, ppreaches)
    
    ppname <- unique(mdat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  
  return(dataoutput)
  
  #removed outlier procedure first. Due to circular statistics, mean and sd now differ.
  #typical outlier removal procedure would not be valid in this case
}

plotCheckGroupCircFreq <- function(groups = c('30', '60'), set){
  
  for(group in groups){
    #dat <- getGroupCircFreq(group = group, set = set)
    dat <- getCheckGroupCircularLC(group=group, set=set)
    dat1 <- getCheckGroupStep1Samp(group=group, set=set)
    if(set == 'fa2020'){
      pdf(sprintf("data/mirrorreversal-fall/doc/fig/DistributionbyStep1_%sCircular.pdf", group))
    } else if (set == 'su2020'){
      pdf(sprintf("data/mReversalNewAlpha3-master/doc/fig/DistributionbyStep1_%sCircular.pdf", group))
    }
    
    #current fix for summer data being non-randomized and not counterbalanced
    #triallist <- dat$trial
    triallist <- c(1:90)
    
    if(group == '30' & set == 'su2020'){
      n <- triallist[seq(1,length(triallist),2)]
      dat <- dat[n,]
      triallist <- dat$trial
    } else if (group == '60' & set == 'su2020'){
      n <- triallist[seq(2,length(triallist),2)]
      dat <- dat[n,]
      triallist <- dat$trial
    }
    
    for(triali in triallist){
      subdat <- dat[which(dat$trial == triali),]
      subdat <- as.numeric(subdat[,2:ncol(subdat)])
      subdat <- as.circular(subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      subdat1 <- dat1[which(dat1$trial == triali),]
      subdat1 <- as.numeric(subdat1[,2:ncol(subdat1)])
      subdatall <- data.frame(subdat1, subdat)
      
      distsubdat <- density.circular(subdat, na.rm = TRUE, bw = 15)
      plot(distsubdat, main = sprintf('%s° Target: Trial %s', group, triali), plot.type = 'circle', shrink=1.20)#1.20 original shrink, can add offset?
      if(group == '30'){
        rd <- as.circular(c(0,120), type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        points.circular(rd, pch = 15, col = '#696969')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#e51636ff', next.points = .025)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#005de4ff', next.points = .05)
        
        lines(distsubdat, points.plot=FALSE, col='#696969', shrink=1.20)
        
        legend(-1.25,-0.95,legend=c('3+ Step1 samples','<3 Step1 samples'),
               col=c('#e51636ff','#005de4ff'),
               pch=1,bty='n',cex=1)
        
        #abline(v = 120, col = 8, lty = 2)
      } else if (group == '60'){
        rd <- as.circular(c(0,60), type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
        points.circular(rd, pch = 15, col = '#696969')
        
        movethrough <- subdatall[which(subdatall$subdat1 == 1),]
        movethrough <- movethrough$subdat
        points.circular(movethrough, pch = 1, col = '#e51636ff', next.points = .025)
        
        nonthrough <- subdatall[which(subdatall$subdat1 == 0),]
        nonthrough <- nonthrough$subdat
        points.circular(nonthrough, pch = 1, col = '#005de4ff', next.points = .05)
        
        lines(distsubdat, points.plot=FALSE, col='#696969', shrink=1.20)
        
        legend(-1.25,-0.95,legend=c('3+ Step1 samples','<3 Step1 samples'),
               col=c('#e51636ff','#005de4ff'),
               pch=1,bty='n',cex=1)
        
        #abline(v = 60, col = 8, lty = 2)
      }
      # axis(1, at = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 330, 300, 360))
      # axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    }
    dev.off()
    
  }
}

#include a plot showing frequency of participants with move throughs per trial
getParticipantMoveThrough <- function(group,set){
  
  
  dat1 <- getCheckGroupStep1Samp(group=group, set=set)
  
  #current fix for summer data being non-randomized and not counterbalanced
  #triallist <- dat$trial
  triallist <- c(1:90)
  
  if(group == '30' & set == 'su2020'){
    n <- triallist[seq(1,length(triallist),2)]
    dat <- dat[n,]
    triallist <- dat$trial
  } else if (group == '60' & set == 'su2020'){
    n <- triallist[seq(2,length(triallist),2)]
    dat <- dat[n,]
    triallist <- dat$trial
  }
  
  trial <- c()
  ppno <- c()
  for(triali in triallist){
    subdat1 <- dat1[which(dat1$trial == triali),]
    subdat1 <- as.numeric(subdat1[,2:ncol(subdat1)])
    ppthrough <- sum(subdat1[which(subdat1 == 1)])
    
    trial <- c(trial, triali)
    ppno <- c(ppno, ppthrough)
  }
  ppmovethrough <- data.frame(trial,ppno)
  
  return(ppmovethrough)
}

plotParticipantMoveThrough <- function(groups=c('30','60'),set){
  
  if(set == 'fa2020'){
    pdf("data/mirrorreversal-fall/doc/fig/ParticipantMoveThrough.pdf", width=11, pointsize = 8.5)
  } else if (set == 'su2020'){
    pdf("data/mReversalNewAlpha3-master/doc/fig/ParticipantMoveThrough.pdf")
  }
  
  for(group in groups){
    data <- getParticipantMoveThrough(group=group, set=set)
    barplot(data$ppno~data$trial, xlab = 'Trial', ylab = 'Frequency of Participants',
            main = sprintf('Participants with move throughs: %s Deg. Target', group), axes = FALSE, #axisnames=FALSE,
            ylim = c(-1,31))
    #axis(1, at = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90))
    axis(2, at = c(0, 2, 4, 6, 8, 10, 20, 30)) #tick marks for y axis
  }
  dev.off()
  
}

#Move through Learning Curves -----
getTrialOneParticipantsWMoveThrough<- function(group, set){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getCheckParticipantCircularLC(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(mdat$trialno))
    mdat$trialno <- trial
    for (triali in trial){
      trialdat <- mdat[which(mdat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$step1_samp <- NA
      }
      mdat[triali,] <- trialdat
    }
    
    ppreaches <- mdat$step1_samp #get reach deviations column from learning curve data
    ppreaches <- ppreaches[1]
    ppname <- unique(mdat$participant)

    ppdat <- data.frame(ppname, ppreaches)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- rbind(dataoutput, ppdat)
    }
  }
  colnames(dataoutput) <- c('participant', 'movethrough')
  return(dataoutput)
}

#Note: Learning Curves to be generated will be based only on 54 pp for 30 deg target and 52 pp for 60 deg target
#This is because this only generates learning curves for participants based on Trial 1
#This also means that trial 2 will be NA, since these participants already made reaches in trial 1,
#and randomization and counterbalancing across trials and participants result in this
getMovedGroupCircularConfInt <- function(groups = c('30', '60'), set, moved){
  for (group in groups){
    ppall <- getTrialOneParticipantsWMoveThrough(group=group, set=set)
    pplist <- ppall[which(ppall$movethrough == moved),] #moved is 1 if movethrough, 0 if not
    pplist <- pplist$participant
    
    dat <- getGroupCircularLC(group=group, set=set)
    trial <- dat$trial
    # get only participants specified in list
    data <- dat[,which(colnames(dat) %in% pplist)]
    data <- cbind(trial, data)
    
    #generate CIs
    
    
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    if(group == '30' & set == 'su2020'){
      n <- trialno[seq(1,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    } else if (group == '60' & set == 'su2020'){
      n <- trialno[seq(2,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    }
    
    confidence <- data.frame()
    
    for(trial in trialno){
      circ_subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      circ_subdat <- as.circular(circ_subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      
      if(length(unique(circ_subdat)) == 1){ #deal with trials with no data at all
        citrial <- as.numeric(c(NA,NA,NA))
      } else{
        citrial <- getCircularConfidenceInterval(data = circ_subdat)
        citrial <- as.numeric(citrial)
      }
      
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_CircularLC_CI.csv', group, moved), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_CircularLC_CI.csv', group, moved), row.names = F) 
      }
    }
  }
  
}

getAlignedMovedGroupCircularConfInt <- function(groups = c('30', '60'), set, moved){
  for (group in groups){
    ppall <- getTrialOneParticipantsWMoveThrough(group=group, set=set)
    pplist <- ppall[which(ppall$movethrough == moved),] #moved is 1 if movethrough, 0 if not
    pplist <- pplist$participant
    
    dat <- getGroupCircularAligned(group=group, set=set)
    trial <- dat$trial
    # get only participants specified in list
    data <- dat[,which(colnames(dat) %in% pplist)]
    data <- cbind(trial, data)
    
    #generate CIs
    
    
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    if(group == '30' & set == 'su2020'){
      n <- trialno[seq(1,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    } else if (group == '60' & set == 'su2020'){
      n <- trialno[seq(2,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    }
    
    confidence <- data.frame()
    
    for(trial in trialno){
      circ_subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      circ_subdat <- as.circular(circ_subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      
      if(length(unique(circ_subdat)) == 1){ #deal with trials with no data at all
        citrial <- as.numeric(c(NA,NA,NA))
      } else{
        citrial <- getCircularConfidenceInterval(data = circ_subdat)
        citrial <- as.numeric(citrial)
      }
      
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_CircularAligned_CI.csv', group, moved), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_CircularAligned_CI.csv', group, moved), row.names = F) 
      }
    }
  }
  
}

getRAEMovedGroupCircularConfInt <- function(groups = c('30', '60'), set, moved){
  for (group in groups){
    ppall <- getTrialOneParticipantsWMoveThrough(group=group, set=set)
    pplist <- ppall[which(ppall$movethrough == moved),] #moved is 1 if movethrough, 0 if not
    pplist <- pplist$participant
    
    dat <- getGroupCircularRAE(group=group, set=set)
    trial <- dat$trial
    # get only participants specified in list
    data <- dat[,which(colnames(dat) %in% pplist)]
    data <- cbind(trial, data)
    
    #generate CIs
    
    
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    if(group == '30' & set == 'su2020'){
      n <- trialno[seq(1,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    } else if (group == '60' & set == 'su2020'){
      n <- trialno[seq(2,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    }
    
    confidence <- data.frame()
    
    for(trial in trialno){
      circ_subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      circ_subdat <- as.circular(circ_subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      
      if(length(unique(circ_subdat)) == 1){ #deal with trials with no data at all
        citrial <- as.numeric(c(NA,NA,NA))
      } else{
        citrial <- getCircularConfidenceInterval(data = circ_subdat)
        citrial <- as.numeric(citrial)
      }
      
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_CircularRAE_CI.csv', group, moved), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_CircularRAE_CI.csv', group, moved), row.names = F) 
      }
    }
  }
  
}

#This one is just for the mirror phase
plotMoveThroughLC <- function(groups = c('30', '60'), moves = c('0','1'), target='inline', set) {
  for(group in groups){
    if (set == 'fa2020'){
      if (target=='svg') {
        svglite(file=sprintf('data/mirrorreversal-fall/doc/fig/Fig6_%s_MoveThroughLC.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
      }
      
      
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(0,91), ylim = c(-10,135), 
           xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("%s degree target location", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
      axis(2, at = c(0, 30, 60, 90, 120)) #tick marks for y axis
      
      for(move in moves){
        #read in files created by getGroupConfidenceInterval in filehandling.R
        groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_CircularLC_CI.csv', group, move))
        
        
        
        colourscheme <- getMoveThroughColourScheme(moves = move)
        #take only first, last and middle columns of file
        lower <- groupconfidence[,1]
        upper <- groupconfidence[,3]
        mid <- groupconfidence[,2]
        
        col <- colourscheme[[move]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        #polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
        polygon(x = c(c(1,3:90), rev(c(1,3:90))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
        meanGroupReaches[[move]] <- mid #use mean to fill in empty list for each group
      }
      
      
      for (move in moves) {
        # plot mean reaches for each group
        col <- colourscheme[[move]][['S']]
        lines(x = c(1,3:90), y = na.omit(meanGroupReaches[[move]]),col=col,lty=1)
      }
      
      #add legend
      legend(60,25,legend=c('no move through','move through'),
             col=c(colourscheme[['0']][['S']],colourscheme[['1']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
      #close everything if you saved plot as svg
      if (target=='svg') {
        dev.off()
      }
      
    } else if (set == 'su2020'){
      #STILL NEED TO FIX....
      if (target=='svg') {
        svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig1B_CircularLC.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
      }
      
      
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(0,46), ylim = c(-10,135), 
           xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
           main = "Rate of learning per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(1, 15, 30, 45)) #tick marks for x axis
      axis(2, at = c(0, 30, 60, 90, 120)) #tick marks for y axis
      
      for(group in groups){
        #read in files created by getGroupConfidenceInterval in filehandling.R
        groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularLC_CI.csv', group))
        
        
        
        colourscheme <- getOnlineColourScheme(groups = group)
        #take only first, last and middle columns of file
        lower <- groupconfidence[,1]
        upper <- groupconfidence[,3]
        mid <- groupconfidence[,2]
        
        col <- colourscheme[[group]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(1:45), rev(c(1:45))), y = c(lower, rev(upper)), border=NA, col=col)
        
        meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
      }
      
      
      for (group in groups) {
        # plot mean reaches for each group
        col <- colourscheme[[group]][['S']]
        lines(meanGroupReaches[[group]],col=col,lty=1)
      }
      
      #add legend
      legend(30,25,legend=c('30° target','60° target'),
             col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
      #close everything if you saved plot as svg
      if (target=='svg') {
        dev.off()
      }
    }
  }
  
  
  
  
}

#But we can also plot for all phases
plotMoveThroughAllTasks <- function(groups = c('30', '60'), moves = c('0','1'), target='inline', set) {
  for (group in groups){
    if(set == 'fa2020'){
      #but we can save plot as svg file
      if (target=='svg'){
        svglite(file=sprintf('data/mirrorreversal-fall/doc/fig/Fig7_%s_MoveThroughAllTasks.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
      }
      
      # create plot
      #meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(0,131), ylim = c(-20,140), 
           xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("%s degree target location", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v= c(20, 110), col = 8, lty = 2)
      axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
      axis(2, at = c(-15, 0, 15, 30, 60, 90, 120)) #tick marks for y axis
      
      for(move in moves){
        #read in files created by getGroupConfidenceInterval in filehandling.R
        groupconfidenceAligned <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_CircularAligned_CI.csv', group, move))
        groupconfidenceLC <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_CircularLC_CI.csv', group, move))
        groupconfidenceRAE <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_CircularRAE_CI.csv', group, move))
        
        
        
        colourscheme <- getMoveThroughColourScheme(moves = move)
        #plot Aligned Data
        #take only first, last and middle columns of file
        lower <- groupconfidenceAligned[,1]
        upper <- groupconfidenceAligned[,3]
        mid <- groupconfidenceAligned[,2]
        
        col <- colourscheme[[move]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(1,3:20), rev(c(1,3:20))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
        col <- colourscheme[[move]][['S']]
        lines(x = c(1,3:20), y = na.omit(mid),col=col,lty=1)
        
        #plot Mirrored Data
        lower <- groupconfidenceLC[,1]
        upper <- groupconfidenceLC[,3]
        mid <- groupconfidenceLC[,2]
        
        col <- colourscheme[[move]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(21,23:110), rev(c(21,23:110))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
        col <- colourscheme[[move]][['S']]
        lines(x = c(21,23:110), y = na.omit(mid),col=col,lty=1)
        
        #plot Wahout Data
        #take only first, last and middle columns of file
        lower <- groupconfidenceRAE[,1]
        upper <- groupconfidenceRAE[,3]
        mid <- groupconfidenceRAE[,2]
        
        col <- colourscheme[[move]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(111,113:130), rev(c(111,113:130))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
        col <- colourscheme[[move]][['S']]
        lines(x = c(111,113:130), y = na.omit(mid),col=col,lty=1)
      }
      
      #add legend
      legend(80,0,legend=c('no move through','move through'),
             col=c(colourscheme[['0']][['S']],colourscheme[['1']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
      #close everything if you saved plot as svg
      if (target=='svg') {
        dev.off()
      }
    } else if (set == 'su2020'){
      #but we can save plot as svg file
      if (target=='svg') {
        svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig3_CircularAllTasks.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
      }
      
      # create plot
      #meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(0,66), ylim = c(-20,140), 
           xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
           main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v= c(10, 55), col = 8, lty = 2)
      axis(1, at = c(1, 5, 11, 25, 40, 56, 60, 65)) #tick marks for x axis
      axis(2, at = c(-15, 0, 15, 30, 60, 90, 120)) #tick marks for y axis
      
      for(group in groups){
        #read in files created by getGroupConfidenceInterval in filehandling.R
        groupconfidenceAligned <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularAligned_CI.csv', group))
        groupconfidenceLC <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularLC_CI.csv', group))
        groupconfidenceRAE <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularRAE_CI.csv', group))
        
        
        
        colourscheme <- getOnlineColourScheme(groups = group)
        #plot Aligned Data
        #take only first, last and middle columns of file
        lower <- groupconfidenceAligned[,1]
        upper <- groupconfidenceAligned[,3]
        mid <- groupconfidenceAligned[,2]
        
        col <- colourscheme[[group]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(1:10), rev(c(1:10))), y = c(lower, rev(upper)), border=NA, col=col)
        col <- colourscheme[[group]][['S']]
        lines(x = c(1:10), y = mid,col=col,lty=1)
        
        #plot Mirrored Data
        lower <- groupconfidenceLC[,1]
        upper <- groupconfidenceLC[,3]
        mid <- groupconfidenceLC[,2]
        
        col <- colourscheme[[group]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(11:55), rev(c(11:55))), y = c(lower, rev(upper)), border=NA, col=col)
        col <- colourscheme[[group]][['S']]
        lines(x = c(11:55), y = mid,col=col,lty=1)
        
        #plot Wahout Data
        #take only first, last and middle columns of file
        lower <- groupconfidenceRAE[,1]
        upper <- groupconfidenceRAE[,3]
        mid <- groupconfidenceRAE[,2]
        
        col <- colourscheme[[group]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(56:65), rev(c(56:65))), y = c(lower, rev(upper)), border=NA, col=col)
        col <- colourscheme[[group]][['S']]
        lines(x = c(56:65), y = mid,col=col,lty=1)
      }
      
      #add legend
      legend(35,0,legend=c('30° target','60° target'),
             col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
      #close everything if you saved plot as svg
      if (target=='svg') {
        dev.off()
      }
    }
  }
  
  
  
}

#Move through Movement Time (Step 1)----
getMoveThroughStep1MT <- function(groups=c('30','60'), set, moved){
  
  for (group in groups){
    ppall <- getTrialOneParticipantsWMoveThrough(group=group, set=set)
    pplist <- ppall[which(ppall$movethrough == moved),] #moved is 1 if movethrough, 0 if not
    pplist <- pplist$participant
    
    dat <- getGroupAllTasksMT(group=group, set=set, step=1) #step 1, but can do other steps as well
    trial <- dat$trial
    # get only participants specified in list
    data <- dat[,which(colnames(dat) %in% pplist)]
    data <- cbind(trial, data)
    
    #generate CIs
    
    
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    if(group == '30' & set == 'su2020'){
      n <- trialno[seq(1,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    } else if (group == '60' & set == 'su2020'){
      n <- trialno[seq(2,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    }
    
    confidence <- data.frame()
    
    for(trial in trialno){
      circ_subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      circ_subdat <- as.circular(circ_subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      
      if(length(unique(circ_subdat)) == 1){ #deal with trials with no data at all
        citrial <- as.numeric(c(NA,NA,NA))
      } else{
        citrial <- getCircularConfidenceInterval(data = circ_subdat)
        citrial <- as.numeric(citrial)
      }
      
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_%s_AllTasksMT_CI_Step1.csv', group, moved), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_AllTasksMT_CI_Step1.csv', group, moved), row.names = F) 
      }
    }
  }
}

plotMoveThroughAllTasksMTStep1 <- function(groups = c('30', '60'), moves = c('0', '1'), target='inline', set) {
  for(group in groups){
    if(set == 'fa2020'){
      #but we can save plot as svg file
      if (target=='svg'){
        svglite(file=sprintf('data/mirrorreversal-fall/doc/fig/Fig8_%s_MoveThroughStep1MT.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
      }
      
      # create plot
      #meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(0,131), ylim = c(-1,11), 
           xlab = "Trial", ylab = "Movement time (s)", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("%s target location", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(1), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v= c(20, 110), col = 8, lty = 2)
      axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
      axis(2, at = c(0, 1, 2, 3, 4, 5)) #tick marks for y axis
      
      for(move in moves){
        #read in files created by getGroupConfidenceInterval in filehandling.R
        groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_%s_AllTasksMT_CI_Step1.csv', group, move))
        
        
        #split up data set for plotting purposes
        groupconfidenceAligned <- groupconfidence[1:20,]
        groupconfidenceLC <- groupconfidence[21:110,]
        groupconfidenceRAE <- groupconfidence[111:130,] 
        
        colourscheme <- getMoveThroughColourScheme(moves = move)
        #plot Aligned Data
        #take only first, last and middle columns of file
        lower <- groupconfidenceAligned[,1]
        upper <- groupconfidenceAligned[,3]
        mid <- groupconfidenceAligned[,2]
        
        col <- colourscheme[[move]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(1,3:20), rev(c(1,3:20))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
        col <- colourscheme[[move]][['S']]
        lines(x = c(1,3:20), y = na.omit(mid),col=col,lty=1)
        
        #plot Mirrored Data
        lower <- groupconfidenceLC[,1]
        upper <- groupconfidenceLC[,3]
        mid <- groupconfidenceLC[,2]
        
        col <- colourscheme[[move]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(21,23:110), rev(c(21,23:110))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
        col <- colourscheme[[move]][['S']]
        lines(x = c(21,23:110), y = na.omit(mid),col=col,lty=1)
        
        #plot Wahout Data
        #take only first, last and middle columns of file
        lower <- groupconfidenceRAE[,1]
        upper <- groupconfidenceRAE[,3]
        mid <- groupconfidenceRAE[,2]
        
        col <- colourscheme[[move]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(111,113:130), rev(c(111,113:130))), y = c(na.omit(lower), rev(na.omit(upper))), border=NA, col=col)
        col <- colourscheme[[move]][['S']]
        lines(x = c(111,113:130), y = na.omit(mid),col=col,lty=1)
      }
      
      #add legend
      legend(80,8,legend=c('no move through','move through'),
             col=c(colourscheme[['0']][['S']],colourscheme[['1']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
      #close everything if you saved plot as svg
      if (target=='svg') {
        dev.off()
      }
      
    } else if (set == 'su2020'){
      #but we can save plot as svg file
      if (target=='svg') {
        svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig4_MovementTime.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
      } 
      
      # create plot
      #meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(0,66), ylim = c(-1,11), 
           xlab = "Trial", ylab = "Movement time (s)", frame.plot = FALSE, #frame.plot takes away borders
           main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(1), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v= c(10, 55), col = 8, lty = 2)
      axis(1, at = c(1, 5, 11, 25, 40, 56, 60, 65)) #tick marks for x axis
      axis(2, at = c(0, 1, 2, 4, 6, 8, 10)) #tick marks for y axis
      
      for(group in groups){
        #read in files created by getGroupConfidenceInterval in filehandling.R
        groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_AllTasksMT_CI.csv', group))
        
        
        #split up data set for plotting purposes
        groupconfidenceAligned <- groupconfidence[1:10,]
        groupconfidenceLC <- groupconfidence[11:55,]
        groupconfidenceRAE <- groupconfidence[56:65,] 
        
        colourscheme <- getOnlineColourScheme(groups = group)
        #plot Aligned Data
        #take only first, last and middle columns of file
        lower <- groupconfidenceAligned[,1]
        upper <- groupconfidenceAligned[,3]
        mid <- groupconfidenceAligned[,2]
        
        col <- colourscheme[[group]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(1:10), rev(c(1:10))), y = c(lower, rev(upper)), border=NA, col=col)
        col <- colourscheme[[group]][['S']]
        lines(x = c(1:10), y = mid,col=col,lty=1)
        
        #plot Mirrored Data
        lower <- groupconfidenceLC[,1]
        upper <- groupconfidenceLC[,3]
        mid <- groupconfidenceLC[,2]
        
        col <- colourscheme[[group]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(11:55), rev(c(11:55))), y = c(lower, rev(upper)), border=NA, col=col)
        col <- colourscheme[[group]][['S']]
        lines(x = c(11:55), y = mid,col=col,lty=1)
        
        #plot Wahout Data
        #take only first, last and middle columns of file
        lower <- groupconfidenceRAE[,1]
        upper <- groupconfidenceRAE[,3]
        mid <- groupconfidenceRAE[,2]
        
        col <- colourscheme[[group]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(c(56:65), rev(c(56:65))), y = c(lower, rev(upper)), border=NA, col=col)
        col <- colourscheme[[group]][['S']]
        lines(x = c(56:65), y = mid,col=col,lty=1)
      }
      
      #add legend
      legend(35,8,legend=c('30° target','60° target'),
             col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
      #close everything if you saved plot as svg
      if (target=='svg') {
        dev.off()
      }
    }
    
  }
  
  
}

# then we can compare step 1 MT to step 2 reachdev for those with move throughs in Trial 1 Mirror
getMoveThroughGroupRDMT <- function(group, set, moved){
  
  ppall <- getTrialOneParticipantsWMoveThrough(group=group, set=set)
  pplist <- ppall[which(ppall$movethrough == moved),] #moved is 1 if movethrough, 0 if not
  pplist <- pplist$participant
  
  dat <- getGroupRDMT(group=group, set=set)
  
  # get only participants specified in list
  data <- dat[which(dat$participant %in% pplist),]
  
  return(data)
}

plotMoveThroughGroupRDMT <- function(group, moves = c('0', '1'), target='inline', set) {
  
  for(move in moves){
    #but we can save plot as svg file
    if (target=='svg' & set == 'su2020') {
      svglite(file=sprintf('data/mReversalNewAlpha3-master/doc/fig/Fig9_%s_%s_MoveThrough_RDMT.svg', group, move), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & set == 'fa2020'){
      svglite(file=sprintf('data/mirrorreversal-fall/doc/fig/Fig9_%s_%s_MoveThrough_RDMT.svg', group, move), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    # create plot
    #meanGroupReaches <- list() #empty list so that it plots the means last
    dat <- getMoveThroughGroupRDMT(group=group, set = set, moved=move)
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(-1,26), ylim=c(-5,190),
         xlab = 'Movement Time, Step 1', ylab = "Circular Reach deviation, Step 2",
         main = sprintf('%s-deg target: Trial 21', group), frame.plot = FALSE, xaxt = 'n', yaxt = 'n')
    axis(1, at = c(0, 5, 10, 15, 20, 25)) #tick marks for x axis
    axis(2, at = c(0, 30, 60, 90, 120, 180)) #tick marks for y axis
    time <- dat$time
    circrd <- dat$circ_rd
    circrd <- abs(dat$circ_rd)
    #ndat <- data.frame(time, circrd)
    #ndat <- ndat[which(ndat$circrd > 90),]
    #circrd <- as.circular(circrd, type='angles', units='degrees', template='none', modulo='asis', zero=0, rotation='counter')
    #points(ndat$time,ndat$circrd)
    #mod1 <- lm(ndat$circrd ~ ndat$time)
    
    points(time,circrd)
    mod1 <- lm(circrd ~ time)
    
    reglinex <- seq(range(time, na.rm = TRUE)[1],range(time, na.rm = TRUE)[2],.1)
    abX <- range(reglinex)
    abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
    lines(abX, abY, col='#343434')
    
    print(summary(mod1))
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
  
  
}

#density or frequency plots: LINEAR-----
# getParticipantFrequency <- function(filename){
#   dat <- handleOneFile(filename = filename)
#   mdat <- dat[which(dat$taskno == 2), ]
#   return(mdat)
# }
# 
# getGroupFrequency <- function(group, set){
#   if (set == 'su2020'){
#     datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
#     #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
#   } else if (set == 'fa2020'){
#     datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
#   }
#   
#   dataoutput<- data.frame() #create place holder
#   for(datafilenum in c(1:length(datafilenames))){
#     if (set == 'su2020'){
#       datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
#     } else if (set == 'fa2020'){
#       datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
#     }
#     cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
#     mdat <- getParticipantFrequency(filename = datafilename)
#     # per target location, get reachdev for corresponding trials
#     
#     trial <- c(1:length(mdat$trialno))
#     mdat$trialno <- trial
#     for (triali in trial){
#       trialdat <- mdat[which(mdat$trialno == triali),]
#       #set reachdev to NA if not the target location we want
#       if (trialdat$targetangle_deg != group){
#         trialdat$reachdeviation_deg <- NA
#       }
#       mdat[triali,] <- trialdat
#     }
#     ppreaches <- mdat$reachdeviation_deg #get reach deviations column from learning curve data
#     ppdat <- data.frame(trial, ppreaches)
#     
#     ppname <- unique(mdat$participant)
#     names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
#     
#     if (prod(dim(dataoutput)) == 0){
#       dataoutput <- ppdat
#     } else {
#       dataoutput <- cbind(dataoutput, ppreaches)
#       names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
#     }
#   }
#   
#   for (trialno in dataoutput$trial){
#     #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
#     ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
#     trialmu <- mean(ndat, na.rm = TRUE)
#     trialsigma <- sd(ndat, na.rm = TRUE)
#     #print(trialsigma)
#     trialclip <- abs(trialmu) + (trialsigma * 2)
#     
#     ndat[which(abs(ndat) > trialclip)] <- NA
#     
#     dataoutput[trialno, 2:ncol(dataoutput)] <- ndat
#   }
#   
#   return(dataoutput)
# }
# 
# plotGroupFrequency <- function(group, set = 'fa2020'){
# 
#   dat <- getGroupFrequency(group = group, set = set)
#   pdf(sprintf("data/mirrorreversal-fall/doc/fig/Distribution_%sTargetLoc.pdf", group))
#   
#   #current fix for summer data being non-randomized and not counterbalanced
#   triallist <- dat$trial
#   
#   if(group == '30' & set == 'su2020'){
#     n <- triallist[seq(1,length(triallist),2)]
#     dat <- dat[n,]
#     triallist <- dat$trial
#   } else if (group == '60' & set == 'su2020'){
#     n <- triallist[seq(2,length(triallist),2)]
#     dat <- dat[n,]
#     triallist <- dat$trial
#   }
#   
#   for(triali in triallist){
#     subdat <- dat[which(dat$trial == triali),]
#     subdat <- as.numeric(subdat[,2:ncol(subdat)])
#     print(densityplot(subdat, width=45, xlim=range(-200,200), xlab = 'angles', ylab="density of participant distribution", 
#                       main = sprintf('%s-deg Target: Trial %s', group, triali),
#                       scales=list(x=list(at=c(-180, -120, -60, 0, 60, 120, 180))),
#                       panel = function(...) {
#                         panel.densityplot(...)
#                         if(group=='30'){
#                           panel.abline(v = 120, lty=2)
#                         } else if (group=='60'){
#                           panel.abline(v = 60, lty=2)
#                         }
#                         
#                       }))
#   }
#   dev.off()
#   
# }

#test reachdev and MT----
#plot reachdev in step 2 over MT in step 1 for trial 21
getParticipantRDMT <- function(filename){
  dfrd <- getParticipantCircularLC(filename = filename)
  dfmt <- handleOneMTFile(filename = filename, step = 1)
  dfmt <- dfmt[which(dfmt$taskno == 2),]
  
  subdfrd <- dfrd[which(dfrd$trialno == 21),]
  subdfmt <- dfmt[which(dfmt$trialno == 21),]
  
  subdat <- data.frame(subdfrd, subdfmt$time)
  colnames(subdat) <- c('trialno', 'targetangle_deg', 'mirror', 'reachdeviation_deg', 'taskno', 'participant', 'circ_rd', 'time')
  
  return(subdat)
}

getGroupRDMT <- function(group, set){
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getParticipantRDMT(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    
    
    #set reachdev to NA if not the target location we want
    if (mdat$targetangle_deg != group){
      mdat$circ_rd <- NA
      mdat$time <- NA
    }
    
    
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- mdat
    } else {
      dataoutput <- rbind(dataoutput, mdat)
      #names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  
  
  
  return(dataoutput)
  
  
}

plotGroupRDMT <- function(group, target='inline', set) {
  
  
  #but we can save plot as svg file
  if (target=='svg' & set == 'su2020') {
    svglite(file=sprintf('data/mReversalNewAlpha3-master/doc/fig/Fig5_RDMT_%s.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & set == 'fa2020'){
    svglite(file=sprintf('data/mirrorreversal-fall/doc/fig/Fig5_RDMT_%s.svg', group), width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #meanGroupReaches <- list() #empty list so that it plots the means last
  dat <- getGroupRDMT(group=group, set = set)
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(-1,26), ylim=c(-250,250),
       xlab = 'Movement Time, Step 1', ylab = "Circular Reach deviation, Step 2",
       main = sprintf('%s-deg target: Trial 21', group), frame.plot = FALSE, xaxt = 'n', yaxt = 'n')
  axis(1, at = c(0, 5, 10, 15, 20, 25)) #tick marks for x axis
  axis(2, at = c(-180, -120, -60, 0, 60, 120, 180)) #tick marks for y axis
  time <- dat$time
  circrd <- dat$circ_rd
  circrd <- as.circular(circrd, type='angles', units='degrees', template='none', modulo='asis', zero=0, rotation='counter')
  points(time,circrd)
  mod1 <- lm(circrd ~ time)
  
  reglinex <- seq(range(time, na.rm = TRUE)[1],range(time, na.rm = TRUE)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Movement Time plots ----
#note: trialMouse.time is in unit of seconds
#plot MT for each step (1,2,3) across trials, per target
handleOneMTFile <- function(filename, step) {
  
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
  taskno <- c()             #trialsNum
  participant <- c()
  time <- c()
  
  # remove empty lines:
  df <- df[which(!is.na(df$trialsNum)),]
  
  # loop through all trials
  for (trialnum in c(1:dim(df)[1])) {
    #print(trialnum)
    s <- convertCellToNumVector(df$step[trialnum])
    m <- df$trialsType[trialnum]
    a <- df$targetangle_deg[trialnum]
    p <- df$participant[trialnum]
    t <- convertCellToNumVector(df$trialMouse.time[trialnum])
    
    # remove stuff that is not step==2
    stepidx = which(s == step)
    t <- t[stepidx]
    startt <- t[1]
    endt <- t[length(t)]
    mt <- endt - startt

    
    # store in vectors:
    trialno <- c(trialno, trialnum)
    targetangle_deg <- c(targetangle_deg, a)
    mirror <-c(mirror, m)
    taskno <- c(taskno, df$trialsNum[trialnum])
    participant <- c(participant, p)
    time <- c(time, mt)
  }
  
  # vectors as data frame columns:
  dfmt <- data.frame(trialno, targetangle_deg, mirror, taskno, participant, time)
  
 
  return(dfmt)
}

getGroupAllTasksMT <- function(group, set, step){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    alldat <- handleOneMTFile(filename = datafilename, step = step)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(alldat$trialno))
    alldat$trialno <- trial
    for (triali in trial){
      trialdat <- alldat[which(alldat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$time <- NA
      }
      alldat[triali,] <- trialdat
    }
    ppmt <- alldat$time #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, ppmt)
    
    ppname <- unique(alldat$participant)
    names(ppdat)[names(ppdat) == 'ppmt'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppmt)
      names(dataoutput)[names(dataoutput) == 'ppmt'] <- ppname
    }
  }
  
  for (trialno in dataoutput$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
    #print(max(ndat, na.rm=T))
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)

    ndat[which(abs(ndat) > trialclip)] <- NA

    dataoutput[trialno, 2:ncol(dataoutput)] <- ndat
  }
  
  return(dataoutput)
  
  #can keep track of deleted trials here, by using the saved csv file or counting NA values in dataoutput
  #write.csv(dataoutput, file='data/mReversalNewAlpha3-master/data/processed/30_learningcurves.csv', row.names = F) 
  #Note: multiple files have no step 2 or have many trials without step 2 in su2020 data
  #These participant files have been removed
  #check for any more NA values:
  #names(which(colSums(is.na(dataoutput))>0))
}

getGroupAllTasksMTConfInt <- function(groups = c('30','60'), type = 't', set, step){
  for(group in groups){
    data <- getGroupAllTasksMT(group = group, set = set, step = step)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    if(group == '30' & set == 'su2020'){
      n <- trialno[seq(1,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    } else if (group == '60' & set == 'su2020'){
      n <- trialno[seq(2,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    }
    
    data1 <- as.matrix(data[,2:dim(data)[2]])
    confidence <- data.frame()
    
    
    for (trial in trialno){
      if (set == 'fa2020'){
        cireaches <- data1[which(data$trial == trial), ]
      } else if (set == 'su2020'){
        cireaches <- as.numeric(data[trial, 2:length(data)])
      }
      
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_AllTasksMT_CI.csv', group), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_AllTasksMT_CI.csv', group), row.names = F) 
      }
    }
  }
}

plotAllTasksMT <- function(groups = c('30', '60'), target='inline', set) {
  
  if(set == 'fa2020'){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file='data/mirrorreversal-fall/doc/fig/Fig4_MovementTime.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    # create plot
    #meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,131), ylim = c(-1,11), 
         xlab = "Trial", ylab = "Movement time (s)", frame.plot = FALSE, #frame.plot takes away borders
         main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(1), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v= c(20, 110), col = 8, lty = 2)
    axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
    axis(2, at = c(0, 1, 2, 4, 6, 8, 10)) #tick marks for y axis
    
    for(group in groups){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_AllTasksMT_CI.csv', group))
      
      
      #split up data set for plotting purposes
      groupconfidenceAligned <- groupconfidence[1:20,]
      groupconfidenceLC <- groupconfidence[21:110,]
      groupconfidenceRAE <- groupconfidence[111:130,] 
      
      colourscheme <- getOnlineColourScheme(groups = group)
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:20), rev(c(1:20))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(1:20), y = mid,col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(21:110), rev(c(21:110))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(21:110), y = mid,col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(111:130), rev(c(111:130))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(111:130), y = mid,col=col,lty=1)
    }
    
    #add legend
    legend(80,8,legend=c('30° target','60° target'),
           col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  } else if (set == 'su2020'){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig4_MovementTime.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } 
    
    # create plot
    #meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,66), ylim = c(-1,11), 
         xlab = "Trial", ylab = "Movement time (s)", frame.plot = FALSE, #frame.plot takes away borders
         main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(1), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v= c(10, 55), col = 8, lty = 2)
    axis(1, at = c(1, 5, 11, 25, 40, 56, 60, 65)) #tick marks for x axis
    axis(2, at = c(0, 1, 2, 4, 6, 8, 10)) #tick marks for y axis
    
    for(group in groups){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_AllTasksMT_CI.csv', group))
     
      
      #split up data set for plotting purposes
      groupconfidenceAligned <- groupconfidence[1:10,]
      groupconfidenceLC <- groupconfidence[11:55,]
      groupconfidenceRAE <- groupconfidence[56:65,] 
      
      colourscheme <- getOnlineColourScheme(groups = group)
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:10), rev(c(1:10))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(1:10), y = mid,col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(11:55), rev(c(11:55))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(11:55), y = mid,col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(56:65), rev(c(56:65))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(56:65), y = mid,col=col,lty=1)
    }
    
    #add legend
    legend(35,8,legend=c('30° target','60° target'),
           col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
  
  
}

#clean baseline aligned----
#baseline should be relatively straightforward (reach towards target)
#we expect participants to have angular reach deviations within the quadrant of targets (0 to 90 degrees)
#with target aligned to zero, this range is -30 degrees to 60 for the 30 deg target
# range is -60 degrees to 30 for the 60 deg target
removeOutlierAlignedReaches <- function(group, set){
  
  #get aligned data for specific group
  data <- getGroupCircularAligned(group = group, set = set)
  #set outlier values (ie out of quadrant) to NA, this depends on target location
  for (trialno in data$trial){
    #go through each trial, replace outlier values with NA
    ndat <- as.numeric(data[trialno, 2:ncol(data)])
    if (group == '30'){
      ndat[which(ndat < -30 | ndat > 60)] <- NA
    } else if (group == '60'){
      ndat[which(ndat < -60 | ndat > 30)] <- NA
    }
    data[trialno, 2:ncol(data)] <- ndat
  }
  return(data)
}

#then if each participant is left with just less than 5 trials for each target, remove participant from analysis
getParticipantsOutlierAligned <- function(group, set){
  data <- removeOutlierAlignedReaches(group=group, set=set)
  ppremove <- c()
  for(pp in 2:ncol(data)){
    ndat <- data[,pp]
    clip <- sum(!is.na(ndat))
    if(clip < 5){
      ppname <- colnames(data[pp])
      ppremove <- c(ppremove, ppname)
    }
  }
  return(ppremove)
}

#PPs with below 5 trials per target
#30 deg target: 216709 (4 trials), 218425 (2 trials), 219301 (4 trials), 221128 (4 trials)
#60 deg target: 218425 (0 trials)


#learning curves: Circular Aligned----
getParticipantCircularAligned <- function(filename){
  
  #get Aligned biases
  dat <- handleOneFile(filename = filename)
  dat$circ_rd <- as.circular(dat$reachdeviation_deg, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  
  adat <- dat[which(dat$taskno == 1), ]

  return(adat)
}

getGroupCircularAligned <- function(group, set){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    adat <- getParticipantCircularAligned(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(adat$trialno))
    adat$trialno <- trial
    for (triali in trial){
      trialdat <- adat[which(adat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$reachdeviation_deg <- NA
      }
      adat[triali,] <- trialdat
    }
    ppreaches <- adat$reachdeviation_deg #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, ppreaches)
    
    ppname <- unique(adat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  return(dataoutput)
  #write.csv(dataoutput, file='data/mReversalNewAlpha3-master/data/processed/learningcurves.csv', row.names = F) 
  #Note: multiple files have no step 2 or have many trials without step 2
  #These participant files have been removed
  #check for any more NA values:
  #names(which(colSums(is.na(dataoutput))>0))
}

getGroupCircularAlignedConfInt <- function(groups = c('30','60'), set){
  for(group in groups){
    #data <- getGroupCircularAligned(group=group, set=set)
    # use cleaned aligned trials (i.e. only trials with reaches in correct quadrant)
    data <- removeOutlierAlignedReaches(group=group, set=set)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    if(group == '30' & set == 'su2020'){
      n <- trialno[seq(1,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    } else if (group == '60' & set == 'su2020'){
      n <- trialno[seq(2,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    }
    confidence <- data.frame()
    
    for(trial in trialno){
      circ_subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      circ_subdat <- as.circular(circ_subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      
      citrial <- getCircularConfidenceInterval(data = circ_subdat)
      citrial <- as.numeric(citrial)
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }

      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularAligned_CI.csv', group), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularAligned_CI.csv', group), row.names = F) 
      }
    }
  }
}

plotCircularAligned <- function(groups = c('30', '60'), target='inline', set) {
  
  if (set == 'fa2020'){
    if (target=='svg') {
      svglite(file='data/mirrorreversal-fall/doc/fig/Fig1C_CircularAligned.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,21), ylim = c(-20,20), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Rate of learning per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(1, 5, 10, 15, 20)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5, 10, 15)) #tick marks for y axis
    
    for(group in groups){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularAligned_CI.csv', group))
      
      
      
      colourscheme <- getOnlineColourScheme(groups = group)
      #take only first, last and middle columns of file
      lower <- groupconfidence[,1]
      upper <- groupconfidence[,3]
      mid <- groupconfidence[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:20), rev(c(1:20))), y = c(lower, rev(upper)), border=NA, col=col)
      
      meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
    }
    
    
    for (group in groups) {
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      lines(meanGroupReaches[[group]],col=col,lty=1)
    }
    
    #add legend
    legend(15,-10,legend=c('30° target','60° target'),
           col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  } else if (set == 'su2020'){
    
    if (target=='svg') {
      svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig1C_CircularAligned.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,11), ylim = c(-20,20), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Rate of learning per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(1, 3, 5, 7, 10)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5, 10, 15)) #tick marks for y axis
    
    for(group in groups){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularAligned_CI.csv', group))
      
      
      
      colourscheme <- getOnlineColourScheme(groups = group)
      #take only first, last and middle columns of file
      lower <- groupconfidence[,1]
      upper <- groupconfidence[,3]
      mid <- groupconfidence[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:10), rev(c(1:10))), y = c(lower, rev(upper)), border=NA, col=col)
      
      meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
    }
    
    
    for (group in groups) {
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      lines(meanGroupReaches[[group]],col=col,lty=1)
    }
    
    #add legend
    legend(7,-10,legend=c('30° target','60° target'),
           col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
  
}

#learning curves: Circular----
getParticipantCircularLC <- function(filename){
  
  #first, implement baseline correction
  #get Aligned biases
  dat <- handleOneFile(filename = filename)
  dat$circ_rd <- as.circular(dat$reachdeviation_deg, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  
  adat <- dat[which(dat$taskno == 1), ]
  # use cleaned baseline data (reaches in correct quadrant)
  for (trialno in adat$trialno){
    #go through each trial, replace outlier values with NA
    subadat <- adat[trialno,]
    if (subadat$targetangle_deg == '30'){
      subadat$circ_rd[which(subadat$circ_rd < -30 | subadat$circ_rd > 60)] <- NA
    } else if (subadat$targetangle_deg == '60'){
      subadat$circ_rd[which(subadat$circ_rd < -60 | subadat$circ_rd > 30)] <- NA
    }
    adat[trialno, ] <- subadat
  }
  
  biases <- aggregate(circ_rd ~ targetangle_deg, data= adat, FUN = median.circular) 
  
  mdat <- dat[which(dat$taskno == 2),]
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
    bias<- biases[biasno, 'circ_rd'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    mdat$circ_rd[which(mdat$targetangle_deg == target)] <- mdat$circ_rd[which(mdat$targetangle_deg == target)] - bias
    
  }
  return(mdat)
}

getGroupCircularLC <- function(group, set){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getParticipantCircularLC(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(mdat$trialno))
    mdat$trialno <- trial
    for (triali in trial){
      trialdat <- mdat[which(mdat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$circ_rd <- NA
      }
      mdat[triali,] <- trialdat
    }
    ppreaches <- mdat$circ_rd #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, ppreaches)
    
    ppname <- unique(mdat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  
  # for (trialno in dataoutput$trial){
  #   #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
  #   ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
  #   circ_ndat <- as.circular(ndat, type='angles', units='degrees')
  #   trialmu <- mean.circular(circ_ndat, na.rm=TRUE)
  #   trialsigma <- sd.circular(circ_ndat, na.rm=TRUE)
  #   trialclip <- abs(trialmu) + (trialsigma * 2)
  #   
  #   ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
  #   trialmu <- mean(ndat, na.rm = TRUE)
  #   trialsigma <- sd(ndat, na.rm = TRUE)
  #   #print(trialsigma)
  #   trialclip <- abs(trialmu) + (trialsigma * 2)
  #   
  #   ndat[which(abs(ndat) > trialclip)] <- NA
  #   
  #   dataoutput[trialno, 2:ncol(dataoutput)] <- ndat
  # }
  
  return(dataoutput)
  
  #removed outlier procedure first. Due to circular statistics, mean and sd now differ.
  #typical outlier removal procedure would not be valid in this case
}

getGroupCircularConfInt <- function(groups = c('30','60'), set){
  for(group in groups){
    data <- getGroupCircularLC(group=group, set=set)
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    if(group == '30' & set == 'su2020'){
      n <- trialno[seq(1,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    } else if (group == '60' & set == 'su2020'){
      n <- trialno[seq(2,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    }
    
    confidence <- data.frame()
    
    for(trial in trialno){
      circ_subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      circ_subdat <- as.circular(circ_subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      
      citrial <- getCircularConfidenceInterval(data = circ_subdat)
      citrial <- as.numeric(citrial)
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularLC_CI.csv', group), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularLC_CI.csv', group), row.names = F) 
      }
    }
  }
}

plotCircularLC <- function(groups = c('30', '60'), target='inline', set) {
  
  if (set == 'fa2020'){
    if (target=='svg') {
      svglite(file='data/mirrorreversal-fall/doc/fig/Fig1B_CircularLC.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,91), ylim = c(-10,135), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Rate of learning per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
    axis(2, at = c(0, 30, 60, 90, 120)) #tick marks for y axis
    
    for(group in groups){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularLC_CI.csv', group))
      
      
      
      colourscheme <- getOnlineColourScheme(groups = group)
      #take only first, last and middle columns of file
      lower <- groupconfidence[,1]
      upper <- groupconfidence[,3]
      mid <- groupconfidence[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
      
      meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
    }
    
    
    for (group in groups) {
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      lines(meanGroupReaches[[group]],col=col,lty=1)
    }
    
    #add legend
    legend(60,25,legend=c('30° target','60° target'),
           col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  } else if (set == 'su2020'){
    
    if (target=='svg') {
      svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig1B_CircularLC.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,46), ylim = c(-10,135), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Rate of learning per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(1, 15, 30, 45)) #tick marks for x axis
    axis(2, at = c(0, 30, 60, 90, 120)) #tick marks for y axis
    
    for(group in groups){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularLC_CI.csv', group))
      
      
      
      colourscheme <- getOnlineColourScheme(groups = group)
      #take only first, last and middle columns of file
      lower <- groupconfidence[,1]
      upper <- groupconfidence[,3]
      mid <- groupconfidence[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:45), rev(c(1:45))), y = c(lower, rev(upper)), border=NA, col=col)
      
      meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
    }
    
    
    for (group in groups) {
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      lines(meanGroupReaches[[group]],col=col,lty=1)
    }
    
    #add legend
    legend(30,25,legend=c('30° target','60° target'),
           col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }

  
}

#Learning Curves: Circular RAE----
getParticipantCircularRAE <- function(filename){
  
  #first, implement baseline correction
  #get Aligned biases
  dat <- handleOneFile(filename = filename)
  dat$circ_rd <- as.circular(dat$reachdeviation_deg, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  
  adat <- dat[which(dat$taskno == 1), ]
  # use cleaned baseline data (reaches in correct quadrant)
  for (trialno in adat$trialno){
    #go through each trial, replace outlier values with NA
    subadat <- adat[trialno,]
    if (subadat$targetangle_deg == '30'){
      subadat$circ_rd[which(subadat$circ_rd < -30 | subadat$circ_rd > 60)] <- NA
    } else if (subadat$targetangle_deg == '60'){
      subadat$circ_rd[which(subadat$circ_rd < -60 | subadat$circ_rd > 30)] <- NA
    }
    adat[trialno, ] <- subadat
  }
  
  biases <- aggregate(circ_rd ~ targetangle_deg, data= adat, FUN = median.circular) 
  
  mdat <- dat[which(dat$taskno == 3),]
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
    bias<- biases[biasno, 'circ_rd'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    mdat$circ_rd[which(mdat$targetangle_deg == target)] <- mdat$circ_rd[which(mdat$targetangle_deg == target)] - bias
    
  }
  return(mdat)
}

getGroupCircularRAE <- function(group, set){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getParticipantCircularRAE(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(mdat$trialno))
    mdat$trialno <- trial
    for (triali in trial){
      trialdat <- mdat[which(mdat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$circ_rd <- NA
      }
      mdat[triali,] <- trialdat
    }
    ppreaches <- mdat$circ_rd #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, ppreaches)
    
    ppname <- unique(mdat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  
  # for (trialno in dataoutput$trial){
  #   #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
  #   ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
  #   circ_ndat <- as.circular(ndat, type='angles', units='degrees')
  #   trialmu <- mean.circular(circ_ndat, na.rm=TRUE)
  #   trialsigma <- sd.circular(circ_ndat, na.rm=TRUE)
  #   trialclip <- abs(trialmu) + (trialsigma * 2)
  #   
  #   ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
  #   trialmu <- mean(ndat, na.rm = TRUE)
  #   trialsigma <- sd(ndat, na.rm = TRUE)
  #   #print(trialsigma)
  #   trialclip <- abs(trialmu) + (trialsigma * 2)
  #   
  #   ndat[which(abs(ndat) > trialclip)] <- NA
  #   
  #   dataoutput[trialno, 2:ncol(dataoutput)] <- ndat
  # }
  
  return(dataoutput)
  
  #removed outlier procedure first. Due to circular statistics, mean and sd now differ.
  #typical outlier removal procedure would not be valid in this case
}

getGroupCircularRAEConfInt <- function(groups = c('30','60'), set){
  for(group in groups){
    data <- getGroupCircularRAE(group=group, set=set)
    
    #current fix for summer data being non-randomized and not counterbalanced
    trialno <- data$trial
    
    if(group == '30' & set == 'su2020'){
      n <- trialno[seq(1,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    } else if (group == '60' & set == 'su2020'){
      n <- trialno[seq(2,length(trialno),2)]
      data <- data[n,]
      trialno <- c(1:nrow(data))
    }
    confidence <- data.frame()
    
    for(trial in trialno){
      circ_subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
      circ_subdat <- as.circular(circ_subdat, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
      
      citrial <- getCircularConfidenceInterval(data = circ_subdat)
      citrial <- as.numeric(citrial)
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularRAE_CI.csv', group), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularRAE_CI.csv', group), row.names = F) 
      }
    }
  }
}

plotCircularRAE <- function(groups = c('30', '60'), target='inline', set) {
  
  
  if (set == 'fa2020'){
    if (target=='svg') {
      svglite(file='data/mirrorreversal-fall/doc/fig/Fig2B_CircularRAE.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,21), ylim = c(-20,20), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Rate of de-adaptation per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(1, 5, 10, 15, 20)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5, 10, 15)) #tick marks for y axis
    
    for(group in groups){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularRAE_CI.csv', group))
      
      
      
      colourscheme <- getOnlineColourScheme(groups = group)
      #take only first, last and middle columns of file
      lower <- groupconfidence[,1]
      upper <- groupconfidence[,3]
      mid <- groupconfidence[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:20), rev(c(1:20))), y = c(lower, rev(upper)), border=NA, col=col)
      
      meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
    }
    
    
    for (group in groups) {
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      lines(meanGroupReaches[[group]],col=col,lty=1)
    }
    
    #add legend
    legend(15,-10,legend=c('30° target','60° target'),
           col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  } else if (set == 'su2020'){
    
    if (target=='svg') {
      svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig2B_CircularRAE.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,11), ylim = c(-20,20), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Rate of de-adaptation per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(1, 3, 5, 7, 10)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5, 10, 15)) #tick marks for y axis
    
    for(group in groups){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularRAE_CI.csv', group))
      
      
      
      colourscheme <- getOnlineColourScheme(groups = group)
      #take only first, last and middle columns of file
      lower <- groupconfidence[,1]
      upper <- groupconfidence[,3]
      mid <- groupconfidence[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:10), rev(c(1:10))), y = c(lower, rev(upper)), border=NA, col=col)
      
      meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
    }
    
    
    for (group in groups) {
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      lines(meanGroupReaches[[group]],col=col,lty=1)
    }
    
    #add legend
    legend(7,-10,legend=c('30° target','60° target'),
           col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
  
  
}

#Learning curves: Circular ALL TASKS ----
# getParticipantCircularAllTasks <- function(filename){
#   
#   adat <- getParticipantCircularAligned(filename = filename)
#   mdat <- getParticipantCircularLC(filename = filename)
#   wdat <- getParticipantCircularRAE(filename = filename)
#   
#   alldat <- rbind(adat,mdat,wdat)
#   
#   return(alldat)
# }
# 
# getGroupCircularAllTasks<- function(group, set){
#   
#   if (set == 'su2020'){
#     datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
#     #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
#   } else if (set == 'fa2020'){
#     datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
#   }
#   
#   dataoutput<- data.frame() #create place holder
#   for(datafilenum in c(1:length(datafilenames))){
#     if (set == 'su2020'){
#       datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
#     } else if (set == 'fa2020'){
#       datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
#     }
#     cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
#     alldat <- getParticipantCircularAllTasks(filename = datafilename)
#     # per target location, get reachdev for corresponding trials
#     
#     trial <- c(1:length(alldat$trialno))
#     alldat$trialno <- trial
#     for (triali in trial){
#       trialdat <- alldat[which(alldat$trialno == triali),]
#       #set reachdev to NA if not the target location we want
#       if (trialdat$targetangle_deg != group){
#         trialdat$reachdeviation_deg <- NA
#       }
#       alldat[triali,] <- trialdat
#     }
#     ppreaches <- alldat$reachdeviation_deg #get reach deviations column from learning curve data
#     ppdat <- data.frame(trial, ppreaches)
#     
#     ppname <- unique(alldat$participant)
#     names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
#     
#     if (prod(dim(dataoutput)) == 0){
#       dataoutput <- ppdat
#     } else {
#       dataoutput <- cbind(dataoutput, ppreaches)
#       names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
#     }
#   }
#   # 
#   # for (trialno in dataoutput$trial){
#   #   #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
#   #   ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
#   #   trialmu <- mean(ndat, na.rm = TRUE)
#   #   trialsigma <- sd(ndat, na.rm = TRUE)
#   #   #print(trialsigma)
#   #   trialclip <- abs(trialmu) + (trialsigma * 2)
#   #   
#   #   ndat[which(abs(ndat) > trialclip)] <- NA
#   #   
#   #   dataoutput[trialno, 2:ncol(dataoutput)] <- ndat
#   # }
#   
#   return(dataoutput)
#   
#   #can keep track of deleted trials here, by using the saved csv file or counting NA values in dataoutput
#   #write.csv(dataoutput, file='data/mReversalNewAlpha3-master/data/processed/30_learningcurves.csv', row.names = F) 
#   #Note: multiple files have no step 2 or have many trials without step 2 in su2020 data
#   #These participant files have been removed
#   #check for any more NA values:
#   #names(which(colSums(is.na(dataoutput))>0))
# }
# 
# getGroupCircularAllTasksConfInt <- function(groups = c('30','60'), set){
#   for(group in groups){
#     data <- getGroupCircularAllTasks(group=group, set=set)
#     
#     trialno <- data$trial
#     confidence <- data.frame()
#     
#     for(trial in trialno){
#       circ_subdat <- as.numeric(data[trial, 2:length(data)]) #get just the values, then make the circular again
#       circ_subdat <- as.circular(circ_subdat, type='angles', units='degrees')
#       #generates bootstrapped CIs for parameters of a von Mises distribution
#       #circular analogue of a normal distribution
#       CI <- mle.vonmises.bootstrap.ci(circ_subdat, reps=1000, alpha = 0.05) #default reps are 1000 and alpha at .05
#       
#       CIlow <- CI[['mu.ci']][1]
#       CIhigh <- CI[['mu.ci']][2]
#       CImid <- mean.circular(circ_subdat, na.rm=TRUE)
#       citrial <- data.frame(CIlow, CImid,CIhigh, row.names=NULL)
#       
#       if (prod(dim(confidence)) == 0){
#         confidence <- citrial
#       } else {
#         confidence <- rbind(confidence, citrial)
#       }
#       if (set == 'su2020'){
#         write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularAllTasks_CI.csv', group), row.names = F) 
#       } else if (set == 'fa2020'){
#         write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularAllTasks_CI.csv', group), row.names = F) 
#       }
#     }
#   }
# }

#Alternatively, plotting all tasks together would be easier 
#if we just take Circular Aligned, LC, and RAE together (i.e. their CI files saved)
plotCircularAllTasks <- function(groups = c('30', '60'), target='inline', set) {
  
  if(set == 'fa2020'){
    #but we can save plot as svg file
    if (target=='svg'){
      svglite(file='data/mirrorreversal-fall/doc/fig/Fig3_CircularAllTasks.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    # create plot
    #meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,131), ylim = c(-20,140), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Reaches across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v= c(20, 110), col = 8, lty = 2)
    axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
    axis(2, at = c(-15, 0, 15, 30, 60, 90, 120)) #tick marks for y axis
    
    for(group in groups){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidenceAligned <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularAligned_CI.csv', group))
      groupconfidenceLC <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularLC_CI.csv', group))
      groupconfidenceRAE <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_CircularRAE_CI.csv', group))
      
      
      
      colourscheme <- getOnlineColourScheme(groups = group)
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:20), rev(c(1:20))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(1:20), y = mid,col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(21:110), rev(c(21:110))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(21:110), y = mid,col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(111:130), rev(c(111:130))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(111:130), y = mid,col=col,lty=1)
    }
    
    #add legend
    legend(80,0,legend=c('30° target','60° target'),
           col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  } else if (set == 'su2020'){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig3_CircularAllTasks.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    # create plot
    #meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(0,66), ylim = c(-20,140), 
         xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v= c(10, 55), col = 8, lty = 2)
    axis(1, at = c(1, 5, 11, 25, 40, 56, 60, 65)) #tick marks for x axis
    axis(2, at = c(-15, 0, 15, 30, 60, 90, 120)) #tick marks for y axis
    
    for(group in groups){
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidenceAligned <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularAligned_CI.csv', group))
      groupconfidenceLC <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularLC_CI.csv', group))
      groupconfidenceRAE <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_CircularRAE_CI.csv', group))

      
      
      colourscheme <- getOnlineColourScheme(groups = group)
      #plot Aligned Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceAligned[,1]
      upper <- groupconfidenceAligned[,3]
      mid <- groupconfidenceAligned[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(1:10), rev(c(1:10))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(1:10), y = mid,col=col,lty=1)
      
      #plot Mirrored Data
      lower <- groupconfidenceLC[,1]
      upper <- groupconfidenceLC[,3]
      mid <- groupconfidenceLC[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(11:55), rev(c(11:55))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(11:55), y = mid,col=col,lty=1)
      
      #plot Wahout Data
      #take only first, last and middle columns of file
      lower <- groupconfidenceRAE[,1]
      upper <- groupconfidenceRAE[,3]
      mid <- groupconfidenceRAE[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(c(56:65), rev(c(56:65))), y = c(lower, rev(upper)), border=NA, col=col)
      col <- colourscheme[[group]][['S']]
      lines(x = c(56:65), y = mid,col=col,lty=1)
    }
    
    #add legend
    legend(35,0,legend=c('30° target','60° target'),
           col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
  
  
}

#Aligned session -----
getAlParticipantMirrorLC <- function(filename){
  
  #first, implement baseline correction
  #get Aligned biases
  dat <- handleOneFile(filename = filename)
  adat <- dat[which(dat$taskno == 1), ]
  
  return(adat)
}

getAlGroupMirrorLC <- function(group, set){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    adat <- getAlParticipantMirrorLC(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(adat$trialno))
    adat$trialno <- trial
    for (triali in trial){
      trialdat <- adat[which(adat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$reachdeviation_deg <- NA
      }
      adat[triali,] <- trialdat
    }
    ppreaches <- adat$reachdeviation_deg #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, ppreaches)
    
    ppname <- unique(adat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  return(dataoutput)
  #write.csv(dataoutput, file='data/mReversalNewAlpha3-master/data/processed/learningcurves.csv', row.names = F) 
  #Note: multiple files have no step 2 or have many trials without step 2
  #These participant files have been removed
  #check for any more NA values:
  #names(which(colSums(is.na(dataoutput))>0))
}

getAlGroupMirrorConfInt <- function(groups = c('30','60'), type='t', set){
  for (group in groups){
    data <- getAlGroupMirrorLC(group = group, set=set)
    
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_Aligned_CI.csv', group), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_Aligned_CI.csv', group), row.names = F) 
      }
      
      
    }
    
  }
  
}


#learning curves----
getParticipantMirrorLC <- function(filename){
  
  #first, implement baseline correction
  #get Aligned biases
  dat <- handleOneFile(filename = filename)
  adat <- dat[which(dat$taskno == 1), ]
  biases <- aggregate(reachdeviation_deg ~ targetangle_deg, data= adat, FUN = median) 
  
  mdat <- dat[which(dat$taskno == 2),]
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data

    target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
    bias<- biases[biasno, 'reachdeviation_deg'] #get corresponding reachdev or bias

    #subtract bias from reach deviation for rotated session only
    mdat$reachdeviation_deg[which(mdat$targetangle_deg == target)] <- mdat$reachdeviation_deg[which(mdat$targetangle_deg == target)] - bias

  }
  
  #next, calculate percentage of compensation (since mirror reversal would not have a set magnitude for the perturbation)
  #angles <- unique(mdat$targetangle_deg)
  #mdat['compensate'] <- NA
  
  #for (target in angles){
  #if (target == 30){
  #mdat$reachdeviation_deg[which(mdat$targetangle_deg == 30)] <- ((mdat$reachdeviation_deg[which(mdat$targetangle_deg == 30)])/150)*100
  #mdat$compensate[which(mdat$targetangle_deg == target)] <- 120
  #} else if (target == 60){
  #mdat$reachdeviation_deg[which(mdat$targetangle_deg == 60)] <- ((mdat$reachdeviation_deg[which(mdat$targetangle_deg == 60)])/120)*100
  #mdat$compensate[which(mdat$targetangle_deg == target)] <- 60
  #}
  # }
  
  return(mdat)
}




#Here, each column will have participant code/identifier as header. If some participants do not learn to compensate,
#we can remove them from further analysis using these as identifiers.

getGroupMirrorLC <- function(group, set){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getParticipantMirrorLC(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(mdat$trialno))
    mdat$trialno <- trial
    for (triali in trial){
      trialdat <- mdat[which(mdat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$reachdeviation_deg <- NA
      }
      mdat[triali,] <- trialdat
    }
    ppreaches <- mdat$reachdeviation_deg #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, ppreaches)
    
    ppname <- unique(mdat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  
  for (trialno in dataoutput$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)

    ndat[which(abs(ndat) > trialclip)] <- NA

    dataoutput[trialno, 2:ncol(dataoutput)] <- ndat
  }
  
  return(dataoutput)
  
  #can keep track of deleted trials here, by using the saved csv file or counting NA values in dataoutput
  #write.csv(dataoutput, file='data/mReversalNewAlpha3-master/data/processed/30_learningcurves.csv', row.names = F) 
  #Note: multiple files have no step 2 or have many trials without step 2 in su2020 data
  #These participant files have been removed
  #check for any more NA values:
  #names(which(colSums(is.na(dataoutput))>0))
}

getGroupMirrorConfInt <- function(groups = c('30','60'), type = 't', set){
  for(group in groups){
    data <- getGroupMirrorLC(group = group, set = set)
    
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_Learningcurves_CI.csv', group), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_Learningcurves_CI.csv', group), row.names = F) 
      }
    }
  }
}

plotMirrorLC <- function(groups = c('30', '60'), target='inline', set) {
  
  
  #but we can save plot as svg file
  if (target=='svg' & set == 'su2020') {
    svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig1_learningcurve.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & set == 'fa2020'){
    svglite(file='data/mirrorreversal-fall/doc/fig/Fig1_learningcurve.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-10,125), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Rate of learning per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(0, 30, 60, 90, 120)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    if (set == 'su2020'){
      groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_Learningcurves_CI.csv', group))
    } else if (set == 'fa2020'){
      groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_Learningcurves_CI.csv', group))
    }
   
    
    colourscheme <- getOnlineColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
  }
  
  #add legend
   legend(70,25,legend=c('30° target','60° target'),
         col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Reachaftereffects----

getParticipantMirrorRAE <- function(filename){
  
  #first, implement baseline correction
  #get Aligned biases
  dat <- handleOneFile(filename = filename)
  adat <- dat[which(dat$taskno == 1), ]
  biases <- aggregate(reachdeviation_deg ~ targetangle_deg, data= adat, FUN = median) 
  
  mdat <- dat[which(dat$taskno == 3),]
  #print (biases)
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
    bias<- biases[biasno, 'reachdeviation_deg'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    mdat$reachdeviation_deg[which(mdat$targetangle_deg == target)] <- mdat$reachdeviation_deg[which(mdat$targetangle_deg == target)] - bias
    
  }
  
  #next, calculate percentage of compensation (since mirror reversal would not have a set magnitude for the perturbation)
  #angles <- unique(mdat$targetangle_deg)
  #mdat['compensate'] <- NA
  
  #for (target in angles){
  #if (target == 30){
  #mdat$reachdeviation_deg[which(mdat$targetangle_deg == 30)] <- ((mdat$reachdeviation_deg[which(mdat$targetangle_deg == 30)])/150)*100
  #mdat$compensate[which(mdat$targetangle_deg == target)] <- 120
  #} else if (target == 60){
  #mdat$reachdeviation_deg[which(mdat$targetangle_deg == 60)] <- ((mdat$reachdeviation_deg[which(mdat$targetangle_deg == 60)])/120)*100
  #mdat$compensate[which(mdat$targetangle_deg == target)] <- 60
  #}
  # }
  
  return(mdat)
}

getGroupMirrorRAE <- function(group, set){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getParticipantMirrorRAE(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(mdat$trialno))
    mdat$trialno <- trial
    for (triali in trial){
      trialdat <- mdat[which(mdat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$reachdeviation_deg <- NA
      }
      mdat[triali,] <- trialdat
    }
    ppreaches <- mdat$reachdeviation_deg #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, ppreaches)
    
    ppname <- unique(mdat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  
  for (trialno in dataoutput$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    
    dataoutput[trialno, 2:ncol(dataoutput)] <- ndat
  }
  
  return(dataoutput)
  
  #can keep track of deleted trials here, by using the saved csv file or counting NA values in dataoutput
  #write.csv(dataoutput, file='data/mReversalNewAlpha3-master/data/processed/30_learningcurves.csv', row.names = F) 
  #Note: multiple files have no step 2 or have many trials without step 2
  #These participant files have been removed
  #check for any more NA values:
  #names(which(colSums(is.na(dataoutput))>0))
}

getGroupMirrorRAEConfInt <- function(groups = c('30','60'), type = 't', set){
  for(group in groups){
    data <- getGroupMirrorRAE(group = group, set = set)
    
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_Reachaftereffects_CI.csv', group), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_Reachaftereffects_CI.csv', group), row.names = F) 
      }
      
    }
  }
}

plotMirrorRAE <- function(groups = c('30', '60'), target='inline', set) {
  
  
  #but we can save plot as svg file
  if (target=='svg' & set == 'su2020') {
    svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig3_reachaftereffects.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & set == 'fa2020'){
    svglite(file='data/mirrorreversal-fall/doc/fig/Fig2_reachaftereffects.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,21), ylim = c(-10,5), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Rate of deadaptation per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 5, 10, 15, 20)) #tick marks for x axis
  axis(2, at = c(-10, -5, 0, 5)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    if (set == 'su2020'){
      groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_Reachaftereffects_CI.csv', group))
    } else if (set == 'fa2020'){
      groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_Reachaftereffects_CI.csv', group))
    }
    colourscheme <- getOnlineColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:20), rev(c(1:20))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
  }
  
  #add legend
  legend(15,-7,legend=c('30° target','60° target'),
         col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Learning curves: all tasks----

getParticipantAllTasksLC <- function(filename){
  
  adat <- getAlParticipantMirrorLC(filename = filename)
  mdat <- getParticipantMirrorLC(filename = filename)
  wdat <- getParticipantMirrorRAE(filename = filename)
  
  alldat <- rbind(adat,mdat,wdat)

  return(alldat)
}

getGroupAllTasksLC <- function(group, set){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
    #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/data', pattern = '*.csv')
  }
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    alldat <- getParticipantAllTasksLC(filename = datafilename)
    # per target location, get reachdev for corresponding trials
    
    trial <- c(1:length(alldat$trialno))
    alldat$trialno <- trial
    for (triali in trial){
      trialdat <- alldat[which(alldat$trialno == triali),]
      #set reachdev to NA if not the target location we want
      if (trialdat$targetangle_deg != group){
        trialdat$reachdeviation_deg <- NA
      }
      alldat[triali,] <- trialdat
    }
    ppreaches <- alldat$reachdeviation_deg #get reach deviations column from learning curve data
    ppdat <- data.frame(trial, ppreaches)
    
    ppname <- unique(alldat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  
  for (trialno in dataoutput$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dataoutput[trialno, 2:ncol(dataoutput)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    
    dataoutput[trialno, 2:ncol(dataoutput)] <- ndat
  }
  
  return(dataoutput)
  
  #can keep track of deleted trials here, by using the saved csv file or counting NA values in dataoutput
  #write.csv(dataoutput, file='data/mReversalNewAlpha3-master/data/processed/30_learningcurves.csv', row.names = F) 
  #Note: multiple files have no step 2 or have many trials without step 2 in su2020 data
  #These participant files have been removed
  #check for any more NA values:
  #names(which(colSums(is.na(dataoutput))>0))
}

getGroupAllTasksConfInt <- function(groups = c('30','60'), type = 't', set){
  for(group in groups){
    data <- getGroupAllTasksLC(group = group, set = set)
    
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      if (set == 'su2020'){
        write.csv(confidence, file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_AllTasksLC_CI.csv', group), row.names = F) 
      } else if (set == 'fa2020'){
        write.csv(confidence, file=sprintf('data/mirrorreversal-fall/data/processed/%s_AllTasksLC_CI.csv', group), row.names = F) 
      }
    }
  }
}

plotAllTasksLC <- function(groups = c('30', '60'), target='inline', set) {
  
  
  #but we can save plot as svg file
  if (target=='svg' & set == 'su2020') {
    svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig1A_AllTasksLC.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & set == 'fa2020'){
    svglite(file='data/mirrorreversal-fall/doc/fig/Fig1A_AllTasksLC.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,131), ylim = c(-10,125), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Rate of learning per target location", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(v = c(20, 110), col = 8, lty = 2)
  axis(1, at = c(1, 10, 21, 50, 80, 111, 120, 130)) #tick marks for x axis
  axis(2, at = c(-10, 0, 30, 60, 90, 120)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    if (set == 'su2020'){
      groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_AllTasksLC_CI.csv', group))
    } else if (set == 'fa2020'){
      groupconfidence <- read.csv(file=sprintf('data/mirrorreversal-fall/data/processed/%s_AllTasksLC_CI.csv', group))
    }
    
    
    colourscheme <- getOnlineColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:130), rev(c(1:130))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
  }
  
  #add legend
  legend(80,25,legend=c('30° target','60° target'),
         col=c(colourscheme[['30']][['S']],colourscheme[['60']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Learning curves: percent of compensation----
getParticipantCompLC <- function(filename){
  
  #first, implement baseline correction
  #get Aligned biases
  dat <- handleOneFile(filename = filename)
  adat <- dat[which(dat$taskno == 1), ]
  biases <- aggregate(reachdeviation_deg ~ targetangle_deg, data= adat, FUN = median) 
  
  mdat <- dat[which(dat$taskno == 2),]
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle_deg'] #get corresponding target angle
    bias<- biases[biasno, 'reachdeviation_deg'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    mdat$reachdeviation_deg[which(mdat$targetangle_deg == target)] <- mdat$reachdeviation_deg[which(mdat$targetangle_deg == target)] - bias
    
  }
  
  #next, calculate percentage of compensation (since mirror reversal would not have a set magnitude for the perturbation)
  #angles <- unique(mdat$targetangle_deg)
  #mdat['compensate'] <- NA
  
  #target deg are in polar coordinates
  #reachdeviation_deg is how far hand is from target, not zero. If reachdev is 90, your hand is at around 120 deg
  #to calculate percent of compensation: reachdev/perfect compensation for that target (120 for 30 deg target, 60 for 60 deg target)
  
  #for (target in angles){
  #if (target == 30){
  mdat$reachdeviation_deg[which(mdat$targetangle_deg == 30)] <- ((mdat$reachdeviation_deg[which(mdat$targetangle_deg == 30)])/120)*100
  #mdat$compensate[which(mdat$targetangle_deg == target)] <- 120
  #} else if (target == 60){
  mdat$reachdeviation_deg[which(mdat$targetangle_deg == 60)] <- ((mdat$reachdeviation_deg[which(mdat$targetangle_deg == 60)])/60)*100
  #mdat$compensate[which(mdat$targetangle_deg == target)] <- 60
  #}
  # }
  
  return(mdat)
}

getGroupCompLC <- function(){
  
  datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
  #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getParticipantCompLC(filename = datafilename)
    #mdat <- mdat[which(mdat$targetangle_deg == 60),] #get 30 degrees only
    
    ppreaches <- mdat$reachdeviation_deg #get reach deviations column from learning curve data
    trial <- c(1:length(ppreaches)) #sets up trial column
    ppdat <- data.frame(trial, ppreaches)
    
    ppname <- unique(mdat$participant)
    names(ppdat)[names(ppdat) == 'ppreaches'] <- ppname
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, ppreaches)
      names(dataoutput)[names(dataoutput) == 'ppreaches'] <- ppname
    }
  }
  return(dataoutput)
  #write.csv(dataoutput, file='data/mReversalNewAlpha3-master/data/processed/learningcurves.csv', row.names = F) 
  #Note: multiple files have no step 2 or have many trials without step 2
  #These participant files have been removed
  #check for any more NA values:
  #names(which(colSums(is.na(dataoutput))>0))
}

getGroupCompConfInt <- function(type){
  
  data <- getGroupCompLC()
  
  trialno <- data$trial
  data1 <- as.matrix(data[,2:dim(data)[2]])
  
  confidence <- data.frame()
  
  
  for (trial in trialno){
    cireaches <- data1[which(data$trial == trial), ]
    
    if (type == "t"){
      cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
    
    write.csv(confidence, file='data/mReversalNewAlpha3-master/data/processed/Comp_Learningcurves_CI.csv', row.names = F) 
    
  }
  
}

#Learning curves: learned within 90 trials only----
getGroupLearnedLC <- function(limit = 6){
  dat <- getGroupMirrorLC()
  ndat <- dat[81:90, 2:dim(dat)[2]] #last 10 trials per participant only
  
  #set up vectors
  participant <- c()
  learned_trials <- c()
  
  for(ppno in c(1:dim(ndat)[2])){
    ppdat <- ndat[,ppno]
    learned <- sum(ppdat > 50)
    ppname <- colnames(ndat[ppno])
    
    participant <- c(participant, ppname)
    learned_trials <- c(learned_trials, learned)
  }
  total_learned <- data.frame(participant, learned_trials)
  #return(total_learned)
  
  #visualize this data?
  #barplot(learned_trials~participant, data=total_learned)
  #hist(total_learned$learned_trials)
  #pplearned <- sum(total_learned$learned_trials == 10) #helps to check how many participants had x amount of "learned" trials
  
  #If we set criteria to having at least 6 trials that are in correct quadrant out of the last 10, we have 15 participants.
  #list of these participants:
  pplearned <- as.character(total_learned$participant[which(total_learned$learned_trials >= limit)])
  dataoutput <- data.frame()
  for (pp in pplearned){
    datlearn <- dat[,colnames(dat) == pp]
    
    trial <- c(1:length(datlearn)) #sets up trial column
    ppdat <- data.frame(trial, datlearn)
    
    names(ppdat)[names(ppdat) == 'datlearn'] <- pp
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, datlearn)
      names(dataoutput)[names(dataoutput) == 'datlearn'] <- pp
    }
  }
  return(dataoutput)
}

getGroupLearnedConfInt <- function(type){
  
  data <- getGroupLearnedLC()
  
  trialno <- data$trial
  data1 <- as.matrix(data[,2:dim(data)[2]])
  
  confidence <- data.frame()
  
  
  for (trial in trialno){
    cireaches <- data1[which(data$trial == trial), ]
    
    if (type == "t"){
      cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
    
    write.csv(confidence, file='data/mReversalNewAlpha3-master/data/processed/Learned_LC_CI.csv', row.names = F) 
    
  }
  
}

plotLearnedLC <- function(target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='data/mReversalNewALpha3-master/doc/fig/Fig2_learnedLC.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  
  #read in files created by getGroupConfidenceInterval in filehandling.R
  groupconfidence <- read.csv(file='data/mReversalNewAlpha3-master/data/processed/Learned_LC_CI.csv')
  
  #colourscheme <- getColourScheme(groups = group)
  #take only first, last and middle columns of file
  lower <- groupconfidence[,1]
  upper <- groupconfidence[,3]
  mid <- groupconfidence[,2]
  
  #col <- colourscheme[[group]][['T']] #use colour scheme according to group
  col <- '#c400c42f'
  
  #upper and lower bounds create a polygon
  #polygon creates it from low left to low right, then up right to up left -> use rev
  #x is just trial nnumber, y depends on values of bounds
  polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
  
  #meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  
  
  
  
  # plot mean reaches for each group
  #col <- colourscheme[[group]][['S']]
  col <- '#c400c4ff'
  lines(mid,col=col,lty=1)
  
  
  #add legend
  # legend(70,-100,legend=c('Non-Instructed','Instructed'),
  #        col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}








# renaming files (unused) ----
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