source('ana/shared.R')
source('ana/learningRates.R')

#Path Length
#Every sample has (x,y) coordinate.
# First sample will have distance from origin as sqrt(x^2 + y^2) - called absolute vector
# Path length is total distance (given x and y trajectories) traveled between movement onset and offset.
# One may think that simply calculating absolute vector from endpoint will measure this, but trajectories
# may curve or go in different directions. So we need to account for every sample.But we simply can't
# add absolute vectors across samples (this will include lengths accounted for by previous sample). So,
# every new sample's absolute vector will be calculated using the previous sample as its origin. Then all
# these values are added to come up with a total path length.
#Repeat this process for all trials within one participant. Then show mean measures across participants for every trial.

getParticipantPathLength <- function(group, id, taskno, task){
  dat <- getParticipantTaskData(group=group, id=id, taskno=taskno, task=task)
  #get only selected trials, and the samples selected for this trial
  subdat <- dat[which(dat$trialselected_bool == 1),]
  subdat <- subdat[which(subdat$sampleselected_bool == 1),]
  trials <- unique(dat$trial) #need to be based off of dat, because some trials may not be selected - assign NA instead
  
  alldat <- data.frame()
  for (trialno in trials){
    trialdat <- subdat[which(subdat$trial == trialno),]
    ndat <- data.frame()
    for (idx in 1:nrow(trialdat)){
      if (idx == 1){
        sampx <- trialdat$mousex_cm[idx]
        sampy <- trialdat$mousey_cm[idx]
        absvec <- sqrt(((sampx)^2)+((sampy)^2))
      } else {
        sampx <- trialdat$mousex_cm[idx] - trialdat$mousex_cm[idx-1]
        sampy <- trialdat$mousey_cm[idx] - trialdat$mousey_cm[idx-1]
        absvec <- sqrt(((sampx)^2)+((sampy)^2))
      }
      
      
      if (prod(dim(ndat)) == 0){
        ndat <- absvec
      } else {
        ndat <- rbind(ndat, absvec)
      }
      
    }
    pathlength <- sum(ndat[1:length(ndat)])
    #print(pathlength)
    #print(trialno)
    #dat <- cbind(trialno, pathlength)
    
    if (prod(dim(alldat)) == 0){
      alldat <- pathlength
    } else {
      alldat <- rbind(alldat, pathlength)
    }
  }
  
  return(alldat)
  
}

getAlignedGroupPathLength <- function(group, maxppid){
  
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    pppath <- getParticipantPathLength(group=group, id=participant, taskno = 1, task = 'aligned')
    trial <- c(1:length(pppath))
    dat <- cbind(trial, pppath)
    #print(participant)
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, pppath)
    }
  }
  return(dataoutput)
}

getAlignedPathLengthCI <- function(group, maxppid, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getAlignedGroupPathLength(group = group, maxppid = maxppid)
  data <- as.data.frame(data)
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
    if (group == 'noninstructed'){
      write.csv(confidence, file='data/ALIGNED_noninstructed_CI_pathlength.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ALIGNED_instructed_CI_pathlength.csv', row.names = F)
    }
    
  }
}

plotAlignedPathLength <- function(groups = c('noninstructed'),target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig7_ALIGNED_pathlength.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(5,20), 
       xlab = "Trial", ylab = "Path Length (cm)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Path Length across Trials: Aligned", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 12, 24, 36, 48)) #tick marks for x axis
  axis(2, at = c(5, 10, 15, 20)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ALIGNED_%s_CI_pathlength.csv', group))
    
    colourscheme <- getColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
  }
  
  #add legend
  # legend(70,-100,legend=c('Non-Instructed','Instructed'),
  #        col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTGroupPathLength <- function(group, maxppid){
  
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      pppath <- getParticipantPathLength(group, id=participant, taskno = 11, task = 'rotation')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      pppath <- getParticipantPathLength(group, id=participant, taskno = 5, task = 'rotation')
    }
    
    trial <- c(1:length(pppath))
    dat <- cbind(trial, pppath)
    #print(participant)
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, pppath)
    }
  }
  return(dataoutput)
}

getROTPathLengthCI <- function(group, maxppid, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getROTGroupPathLength(group = group, maxppid = maxppid)
  data <- as.data.frame(data)
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
    if (group == 'noninstructed'){
      write.csv(confidence, file='data/ROT_noninstructed_CI_pathlength.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROT_instructed_CI_pathlength.csv', row.names = F)
    }
    
  }
}

getMIRGroupPathLength <- function(group, maxppid){
  
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      pppath <- getParticipantPathLength(group, id=participant, taskno = 5, task = 'mirror')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      pppath <- getParticipantPathLength(group, id=participant, taskno = 11, task = 'mirror')
    }
    
    trial <- c(1:length(pppath))
    dat <- cbind(trial, pppath)
    #print(participant)
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, pppath)
    }
  }
  return(dataoutput)
}

getMIRPathLengthCI <- function(group, maxppid, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMIRGroupPathLength(group = group, maxppid = maxppid)
  data <- as.data.frame(data)
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
    if (group == 'noninstructed'){
      write.csv(confidence, file='data/MIR_noninstructed_CI_pathlength.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIR_instructed_CI_pathlength.csv', row.names = F)
    }
    
  }
}

plotPTypePathLength <- function(perturb = c('ROT', 'MIR'), group = 'noninstructed', target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig7_NI_pathlength.svg', width=11.5, height=8.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(10,20), 
       xlab = "Trial", ylab = "Path Length (cm)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(h = c(-100,0, 100), col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(10, 12, 14, 16, 18)) #tick marks for y axis
  
  for(ptype in perturb){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_%s_CI_pathlength.csv', ptype, group))
    
    colourscheme <- getPtypeColourScheme(ptype)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[ptype]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[ptype]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (ptype in perturb) {
    # plot mean reaches for each group
    col <- colourscheme[[ptype]][['S']]
    lines(meanGroupReaches[[ptype]],col=col,lty=1)
  }
  
  #add legend
  legend(50,18,legend=c('Visuomotor rotation','Mirror reversal'),
         col=c(colourscheme[['ROT']][['S']],colourscheme[['MIR']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}