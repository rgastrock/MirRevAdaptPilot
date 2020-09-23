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
  
  # loop through all trials
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

getAlGroupMirrorLC <- function(){
  
  datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
  #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    adat <- getParticipantMirrorLC(filename = datafilename)
    #mdat <- mdat[which(mdat$targetangle_deg == 60),] #get 60 degrees only
    
    ppreaches <- adat$reachdeviation_deg #get reach deviations column from learning curve data
    trial <- c(1:length(ppreaches)) #sets up trial column
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

getAlGroupMirrorConfInt <- function(type){
  
  data <- getAlGroupMirrorLC()
  
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
    
    write.csv(confidence, file='data/mReversalNewAlpha3-master/data/processed/Al_Learningcurves_CI.csv', row.names = F) 
    
  }
  
}


#Here, each column will have participant code/identifier as header. If some participants do not learn to compensate,
#we can remove them from further analysis using these as identifiers.

get30GroupMirrorLC <- function(){
  
  datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
  #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getParticipantMirrorLC(filename = datafilename)
    mdat <- mdat[which(mdat$targetangle_deg == 30),] #get 30 degrees only
    
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

get30GroupMirrorConfInt <- function(type){
  
  data <- get30GroupMirrorLC()
  
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
    
    write.csv(confidence, file='data/mReversalNewAlpha3-master/data/processed/30_Learningcurves_CI.csv', row.names = F) 
    
  }
  
}

get60GroupMirrorLC <- function(){
  
  datafilenames <- list.files('data/mReversalNewAlpha3-master/data', pattern = '*.csv')
  #datafilenames <- list.files('data/mirrorreversal-master/data', pattern = '*.csv')
  
  dataoutput<- data.frame() #create place holder
  for(datafilenum in c(1:length(datafilenames))){
    datafilename <- sprintf('data/mReversalNewAlpha3-master/data/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    mdat <- getParticipantMirrorLC(filename = datafilename)
    mdat <- mdat[which(mdat$targetangle_deg == 60),] #get 60 degrees only
    
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

get60GroupMirrorConfInt <- function(type){
  
  data <- get60GroupMirrorLC()
  
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
    
    write.csv(confidence, file='data/mReversalNewAlpha3-master/data/processed/60_Learningcurves_CI.csv', row.names = F) 
    
  }
  
}

plotMirrorLC <- function(groups = c('30', '60'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='data/mReversalNewAlpha3-master/doc/fig/Fig1_learningcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,46), ylim = c(0,125), 
       xlab = "Trial", ylab = "Angular reach deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 60, 120), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 45)) #tick marks for x axis
  axis(2, at = c(0, 30, 60, 90, 120)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/mReversalNewAlpha3-master/data/processed/%s_Learningcurves_CI.csv', group))
    
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
   legend(30,40,legend=c('30-deg target','60-deg target'),
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