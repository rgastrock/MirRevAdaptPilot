source('ana/shared.R')
source('ana/learningRates.R')

#Reaction Time Analysis -----

#start of step 3 is when target signal turns to go
#end of step 3 (when it switches to step 4) is when cursor distance from home is greater than radius (which is 0.5 cm * pixpercm or 35)
#so essentially, RT is difference between time that go signal occurs and time when cursor is half a centimeter away from home position

getRTTrials <- function(group, id, task, taskno){
  #allows for this function to work with each file
  #specify pp id, the task type, and task number
  #note that task type and taskno have to match, depending on present csv files
  #Note to change filepath once data folder is arranged
  # if (id < 10){
  #   dat <- read.csv(file = sprintf('data/pilot/SELECTED/%s/p00%d/p00%d-%d-%s.csv', group, id, id, taskno,task))
  # } else{
  #   dat <- read.csv(file = sprintf('data/pilot/SELECTED/%s/p0%d/p0%d-%d-%s.csv', group, id, id, taskno,task))
  # }
  
  dat <- getParticipantTaskData(group = group, id = id, taskno = taskno, task = task)
  trials <- unique(dat$trial) 
  proportion <- data.frame()
  
  for (trialno in trials){
    
    subndat <- dat[dat$trial == trialno,]
    subndat <- subndat[subndat$step == 3,]
    subndatsamp <- subndat[1,]
    
    if (nrow(subndat)==0){
      reactiontime <- NA #will assign NA if step 3 does not occur
      trial <- trialno
      comment <- 'nostep' #this will help to keep track of how many trials did not have a step later on
      
    } else if(subndatsamp$trialselected_bool == 0){#we only want data that has been selected
      reactiontime <- NA
      trial <- trialno
      comment <- 'unselected'
    } else{
      firststep3 <- subndat[1,]
      laststep3 <- subndat[nrow(subndat),]
      
      step3start <- firststep3$time_ms
      step3end <- laststep3$time_ms
      
      reactiontime <- step3end - step3start
      trial <- trialno
      comment <- 'selected'
      
    }
    feedback <- c(trial, reactiontime, task, comment)
    
    
    if (prod(dim(proportion)) == 0){
      proportion <- feedback
    } else {
      proportion <- rbind(proportion, feedback)
    }
  }
  proportion <- data.frame(proportion, row.names = NULL, stringsAsFactors = F)
  colnames(proportion) <- c('trial', 'reaction_time', 'task', 'comment')
  proportion$participant <- id
  return(proportion)
}

getAlignedGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppRT <- getRTTrials(group = group, id=participant, task = 'aligned', taskno = 1) #for every participant, get RT data
    
    reaction <- as.numeric(ppRT$reaction_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    #task <- rep('AL', length(reaction))
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }

  return(dataoutput)
  
}

#Implement outlier removal
getRTAligned <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getAlignedGroupRTTrials(group=group, maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
   
     if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  

  return(dat)
}

getRTBlockedAlignedConfInt <- function(group, maxppid, type = 't'){
  
  data <- getRTAligned(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 12 trials (they go through each of 12 possible targets in aligned)
  n <- 12;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/ALIGNED_noninstructed_blocked_CI_RT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ALIGNED_instructed_blocked_CI_RT.csv', row.names = F)
    } 
    
  }
  
}

#function below generates CIs for every trial
getRTAlignedConfInt <- function(group, maxppid, type = 't'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRTAligned(group = group, maxppid = maxppid)
  #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
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
      write.csv(confidence, file='data/ALIGNED_noninstructed_CI_RT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ALIGNED_instructed_CI_RT.csv', row.names = F)
    } 
    
  }
  
}

#Do the same for ROTATION

getROTGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
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
      ppRT <- getRTTrials(group=group, id=participant, taskno = 11, task = 'rotation')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getRTTrials(group=group, id=participant, taskno = 5, task = 'rotation')
    }
    
    reaction <- as.numeric(ppRT$reaction_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getRTRot <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getROTGroupRTTrials(group=group, maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getRTBlockedROTConfInt <- function(group, maxppid, type = 't'){
  
  data <- getRTRot(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/ROT_noninstructed_blocked_CI_RT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROT_instructed_blocked_CI_RT.csv', row.names = F)
    } 
    
  }
  
}

#function below generates CIs for every trial
getRTROTConfInt <- function(group, maxppid, type = 't'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRTRot(group = group, maxppid = maxppid)
  #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
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
      write.csv(confidence, file='data/ROT_noninstructed_CI_RT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROT_instructed_CI_RT.csv', row.names = F)
    } 
    
  }
  
}


#Do the same for MIRROR

getMIRGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
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
      ppRT <- getRTTrials(group=group, id=participant, taskno = 5, task = 'mirror')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getRTTrials(group=group, id=participant, taskno = 11, task = 'mirror')
    }
    
    reaction <- as.numeric(ppRT$reaction_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getRTMir <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getMIRGroupRTTrials(group=group, maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getRTBlockedMIRConfInt <- function(group, maxppid, type = 't'){
  
  data <- getRTMir(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/MIR_noninstructed_blocked_CI_RT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIR_instructed_blocked_CI_RT.csv', row.names = F)
    } 
    
  }
  
}

#function below generates CIs for every trial
getRTMIRConfInt <- function(group, maxppid, type = 't'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRTMir(group = group, maxppid = maxppid)
  #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
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
      write.csv(confidence, file='data/MIR_noninstructed_CI_RT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIR_instructed_CI_RT.csv', row.names = F)
    } 
    
  }
  
}

#Do the same for ROTWASH

getROTWASHGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
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
      ppRT <- getRTTrials(group=group, id=participant, taskno = 13, task = 'washout1')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getRTTrials(group=group, id=participant, taskno = 7, task = 'washout0')
    }
    
    reaction <- as.numeric(ppRT$reaction_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getRTRotwash <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getROTWASHGroupRTTrials(group=group, maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getRTBlockedRotwashConfInt <- function(group, maxppid, type = 't'){
  
  data <- getRTRotwash(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/ROTWASH_noninstructed_blocked_CI_RT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROTWASH_instructed_blocked_CI_RT.csv', row.names = F)
    } 
    
  }
  
}

#function below generates CIs for every trial
getRTRotwashConfInt <- function(group, maxppid, type = 't'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRTRotwash(group = group, maxppid = maxppid)
  #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
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
      write.csv(confidence, file='data/ROTWASH_noninstructed_CI_RT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROTWASH_instructed_CI_RT.csv', row.names = F)
    } 
    
  }
  
}

#Do the same for MIRWASH

getMIRWASHGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
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
      ppRT <- getRTTrials(group=group, id=participant, taskno = 7, task = 'washout0')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getRTTrials(group=group, id=participant, taskno = 13, task = 'washout1')
    }
    
    reaction <- as.numeric(ppRT$reaction_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getRTMirwash <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getMIRWASHGroupRTTrials(group=group, maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getRTBlockedMirwashConfInt <- function(group, maxppid, type = 't'){
  
  data <- getRTMirwash(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/MIRWASH_noninstructed_blocked_CI_RT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIRWASH_instructed_blocked_CI_RT.csv', row.names = F)
    } 
    
  }
  
}

#function below generates CIs for every trial
getRTMirwashConfInt <- function(group, maxppid, type = 't'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRTMirwash(group = group, maxppid = maxppid)
  #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
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
      write.csv(confidence, file='data/MIRWASH_noninstructed_CI_RT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIRWASH_instructed_CI_RT.csv', row.names = F)
    } 
    
  }
  
}

plotNIBlockedRT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5_NI_reactiontime.svg', width=11.5, height=8.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_noninstructed_blocked_CI_RT.csv')
  dat2 <- read.csv(file='data/ROT_noninstructed_blocked_CI_RT.csv')
  dat3 <- read.csv(file='data/ROTWASH_noninstructed_blocked_CI_RT.csv')
  dat4 <- read.csv(file='data/MIR_noninstructed_blocked_CI_RT.csv')
  dat5 <- read.csv(file='data/MIRWASH_noninstructed_blocked_CI_RT.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #separate each task, then plot as usual according to blocks
  
  X1 <- seq(1, 4,1)
  X3 <- seq(5,19,1)
  X5 <- seq(20,27,1)
  X7 <- seq(28,42,1)
  X9 <- seq(43,50,1)
  
  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Reaction time (ms)', main = '',
       xlim = c(0,51), ylim = c(0,810))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,5,20,28,43,50))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(0, 200, 400,600,800),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(4.5,19.5,27.5,42.5), col = 8, lty = 2)
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:4], rev(YUp[1:4])), border=NA, col=alpha("#b4b4b4",.5))
  polygon(x = c(X3, rev(X3)), y = c(YLow[5:19], rev(YUp[5:19])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[20:27], rev(YUp[20:27])), border=NA, col="#e516362f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[28:42], rev(YUp[28:42])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[43:50], rev(YUp[43:50])), border=NA, col="#005de42f")
  
  lines(X1,Y[1:4], col = alpha("#000000", 1))#aligned
  lines(X3, Y[5:19], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[20:27], col = alpha("#e51636ff", 1))#rotwashout
  lines(X7, Y[28:42], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[43:50], col = alpha("#005de4ff", 1))#mirwashout
  

  
  #add legend
  legend(27,200,legend=c('Aligned','Rotation and washout','Mirror Reversal and washout'),
         col=c("#000000", "#e51636ff", "#005de4ff"),
         lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIBlockedRT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5_I_reactiontime.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_instructed_blocked_CI_RT.csv')
  dat2 <- read.csv(file='data/ROT_instructed_blocked_CI_RT.csv')
  dat3 <- read.csv(file='data/ROTWASH_instructed_blocked_CI_RT.csv')
  dat4 <- read.csv(file='data/MIR_instructed_blocked_CI_RT.csv')
  dat5 <- read.csv(file='data/MIRWASH_instructed_blocked_CI_RT.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #separate each task, then plot as usual according to blocks
  
  X1 <- seq(1, 4,1)
  X3 <- seq(5,19,1)
  X5 <- seq(20,27,1)
  X7 <- seq(28,42,1)
  X9 <- seq(43,50,1)
  
  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Reaction time (ms)', main = '',
       xlim = c(0,51), ylim = c(0,810))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,5,20,28,43,50))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(0, 200, 400,600,800),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(4.5,19.5,27.5,42.5), col = 8, lty = 2)
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:4], rev(YUp[1:4])), border=NA, col=alpha("#b4b4b4",.5))
  polygon(x = c(X3, rev(X3)), y = c(YLow[5:19], rev(YUp[5:19])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[20:27], rev(YUp[20:27])), border=NA, col="#e516362f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[28:42], rev(YUp[28:42])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[43:50], rev(YUp[43:50])), border=NA, col="#005de42f")
  
  lines(X1,Y[1:4], col = alpha("#000000", 1))#aligned
  lines(X3, Y[5:19], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[20:27], col = alpha("#e51636ff", 1))#rotwashout
  lines(X7, Y[28:42], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[43:50], col = alpha("#005de4ff", 1))#mirwashout
  
  
  
  #add legend
  legend(27,200,legend=c('Aligned','Rotation and washout','Mirror Reversal and washout'),
         col=c("#000000", "#e51636ff", "#005de4ff"),
         lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotBlockedRT <- function(target='inline'){
  #need to indicate non instructed and instructed in title
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5_reactiontime.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(2,1))
  
  plotNIBlockedRT()
  plotIBlockedRT()
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Reaction Time STATS----
#analysis is similar in structure to Reach aftereffects, comparing aligned to ROT and MIR, but we also have washout here

getRTLongFormat <- function(groups = c('noninstructed','instructed'), maxppid = 15, location = 'maxvel'){
  
  for (group in groups){
    #Aligned data
    ALdat <- getRTAligned(group=group,maxppid=maxppid)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(ALdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    ALdat <- as.data.frame(ALdat)
    perturb <- rep('AL', nrow(ALdat))
    ALdat <- cbind(ALdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longALdata <- gather(ALdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longALdata, file=sprintf('data/ALIGNED_%s_RT_long.csv', group), row.names = F)
    
    
    #Rotation data
    ROTdat <- getRTRot(group=group,maxppid=maxppid)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(ROTdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    ROTdat <- as.data.frame(ROTdat)
    perturb <- rep('ROT', nrow(ROTdat))
    ROTdat <- cbind(ROTdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longROTdata <- gather(ROTdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longROTdata, file=sprintf('data/ROT_%s_RT_long.csv', group), row.names = F)
    
    #RotWASH data
    ROTWASHdat <- getRTRotwash(group=group,maxppid=maxppid)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(ROTWASHdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    ROTWASHdat <- as.data.frame(ROTWASHdat)
    perturb <- rep('ROTWASH', nrow(ROTWASHdat))
    ROTWASHdat <- cbind(ROTWASHdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longROTWASHdata <- gather(ROTWASHdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longROTWASHdata, file=sprintf('data/ROTWASH_%s_RT_long.csv', group), row.names = F)
    
    #Mirror data
    MIRdat <- getRTMir(group=group,maxppid=maxppid)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(MIRdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    MIRdat <- as.data.frame(MIRdat)
    perturb <- rep('MIR', nrow(MIRdat))
    MIRdat <- cbind(MIRdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longMIRdata <- gather(MIRdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longMIRdata, file=sprintf('data/MIR_%s_RT_long.csv', group), row.names = F)
    
    #MirWASH data
    MIRWASHdat <- getRTMirwash(group=group,maxppid=maxppid)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(MIRWASHdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    MIRWASHdat <- as.data.frame(MIRWASHdat)
    perturb <- rep('MIRWASH', nrow(MIRWASHdat))
    MIRWASHdat <- cbind(MIRWASHdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longMIRWASHdata <- gather(MIRWASHdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longMIRWASHdata, file=sprintf('data/MIRWASH_%s_RT_long.csv', group), row.names = F)
  }
}

#First, we grab data from aligned, since it is the baseline for all
getRTBlockedAlignedData <- function(group, blockdefs){
  LCaov <- data.frame()
  curves <- read.csv(sprintf('data/ALIGNED_%s_RT_long.csv', group), stringsAsFactors=FALSE)  
  participants <- unique(curves$participant)
  #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
  #curves <- curves[,-1] #take away trial column
  N <- length(participants) #gets the number of participants
  perturb = 'AL'
  #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
  #row.names(blocked) <- participants
  #colnames(blocked) <- names(blockdefs)
  
  perturbtype <- c()
  participant <- c()
  block <- c()
  compensation <- c()
  
  for (pp.idx in c(1:length(participants))) {
    
    pp <- participants[pp.idx] #loop through each participant
    
    for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
      
      blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
      blockstart <- blockdef[1] #either trial 1, 4, or 76
      blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
      #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
      # moved to long format files:
      samples <- c()
      for (trial in c(blockstart:blockend)) {
        # print(which(curves$participant == pp))
        # print(which(curves$participant == pp & curves$trial == trial))
        samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
        
      }
      #print(mean(samples, na.rm=TRUE))
      #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
      perturbtype <- c(perturbtype, perturb)
      participant <- c(participant, pp) #the participant
      block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
      compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
    }
    
  }
  
  GroupLCBlocked <- data.frame(perturbtype,participant,block,compensation)
  
  
  if (prod(dim(LCaov)) == 0){
    LCaov <- GroupLCBlocked
  } else {
    LCaov <- rbind(LCaov, GroupLCBlocked)
  }
  
  #need to make some columns as factors for ANOVA
  LCaov$perturbtype <- as.factor(LCaov$perturbtype)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
}

#Then grab data for washout and perturb types

getRTBlockedPerturbData <- function(perturbations = c('ROT','MIR'), group, blockdefs) {
  #function reads in aftereffects_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  #to include instructed group, just create another for loop here
  for (perturb in perturbations){  
    curves <- read.csv(sprintf('data/%s_%s_RT_long.csv',perturb,group), stringsAsFactors=FALSE)  
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    perturbtype <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        perturbtype <- c(perturbtype, perturb)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(perturbtype,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$perturbtype <- as.factor(LCaov$perturbtype)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}

getRTBlockedWashoutData <- function(perturbations = c('ROTWASH','MIRWASH'), group, blockdefs) {
  #function reads in aftereffects_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  #to include instructed group, just create another for loop here
  for (perturb in perturbations){  
    curves <- read.csv(sprintf('data/%s_%s_RT_long.csv',perturb, group), stringsAsFactors=FALSE)  
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    perturbtype <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        perturbtype <- c(perturbtype, perturb)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(perturbtype,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$perturbtype <- as.factor(LCaov$perturbtype)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}


#Note that blockdefs will differ depending on trial type
# Aligned: blockdefs <- list('first'=c(1,12),'second'=c(13,12),'last'=c(37,12))
# WASH:  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6))
# PERTURB:  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6))
# we compare all first blocks of perturb and washout types to last block of aligned

RTt.test <- function(group) {
  
  blockdefs <- list('first'=c(1,12),'second'=c(13,12),'last'=c(37,12))
  LC4test1 <- getRTBlockedAlignedData(group=group,blockdefs=blockdefs)
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  LC4test2 <- getRTBlockedPerturbData(group=group,blockdefs=blockdefs)
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6)) #6 trials per block
  LC4test3 <- getRTBlockedWashoutData(group=group,blockdefs=blockdefs)
  
  LC4test <- rbind(LC4test1, LC4test2, LC4test3)
  LC4test$participant <- as.factor(LC4test$participant)
  
  ALdat <- LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'AL'),]
  ROTdat <- LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'ROT'),]
  MIRdat <-LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'MIR'),]
  ROTWASHdat <-LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'ROTWASH'),]
  MIRWASHdat <-LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'MIRWASH'),]
  
  cat('Aligned (last block) compared to Rotation (first block):\n')
  print(t.test(ALdat$compensation, ROTdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, ROTdat$compensation))
  
  cat('Aligned (last block) compared to Mirror (first block):\n')
  print(t.test(ALdat$compensation, MIRdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, MIRdat$compensation))
  
  cat('Aligned (last block) compared to Rotation Washout (first block):\n')
  print(t.test(ALdat$compensation, ROTWASHdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, ROTWASHdat$compensation))
  
  cat('Aligned (last block) compared to Mirror Washout (first block):\n')
  print(t.test(ALdat$compensation, MIRWASHdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, MIRWASHdat$compensation))
  
  #I add a comparison of last block of ROT and MIR compared to aligned, since block 1 for these differed.
  ROTdatlast <- LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'ROT'),]
  MIRdatlast <-LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'MIR'),]
  
  
  cat('Aligned (last block) compared to Rotation (last block):\n')
  print(t.test(ALdat$compensation, ROTdatlast$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, ROTdatlast$compensation))
  
  cat('Aligned (last block) compared to Mirror (last block):\n')
  print(t.test(ALdat$compensation, MIRdatlast$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, MIRdatlast$compensation))
  
}

# We only see significant differences for ROT and MIR but not the washouts. So we'll only explore ROT and MIR further.

reactiontimeANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- getRTBlockedPerturbData(group=group,blockdefs=blockdefs)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=c(perturbtype, block),type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

RTComparisonMeans <- function(group){
  
  #can plot interaction just to eyeball it:
  #plot(interactionMeans(lm(compensation ~ block * perturbtype, data=LC4aov), factors=c('perturbtype', 'block'), atx='block'))
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6))
  
  LC4aov <- getRTBlockedPerturbData(group=group,blockdefs=blockdefs) 
  secondAOV <- aov_ez("participant","compensation",LC4aov,within=c("perturbtype","block"))
  
  #nice(secondAOV, correction = 'none') #correction set to none since first AOV reveals no violation of sphericity
  #summary(secondAOV) #shows all results
  #run code above for figuring out df
  #output is the same
  #follow-ups using emmeans
  
  cellmeans <- emmeans(secondAOV,specs=c('perturbtype','block'))
  #cellmeans <- lsmeans(secondAOV$aov,specs=c('perturbtype','block'))
  print(cellmeans)
}

RTComparisonsAllBlocks <- function(group,method='bonferroni'){
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6))
  
  LC4aov <- getRTBlockedPerturbData(group=group,blockdefs=blockdefs) 
  secondAOV <- aov_ez("participant","compensation",LC4aov,within=c("perturbtype","block"))
  #based on cellmeans, confidence intervals and plots give us an idea of what contrasts we want to compare
  
  MIR_firstvsMIR_second  <- c(1,0,-1,0,0,0)
  MIR_firstvsMIR_last    <- c(1,0,0,0,-1,0)
  ROT_firstvsROT_second  <- c(0,1,0,-1,0,0)
  ROT_firstvsROT_last    <- c(0,1,0,0,0,-1)
  ROT_firstvsMIR_first   <- c(1,-1,0,0,0,0)
  ROT_secondvsMIR_second <- c(0,0,1,-1,0,0)
  ROT_lastvsMIR_last     <- c(0,0,0,0,1,-1)
  
  contrastList <- list('Block1: MIR vs. Block2: MIR'=MIR_firstvsMIR_second, 'Block1: MIR vs. Block3: MIR'=MIR_firstvsMIR_last,
                       'Block1: ROT vs. Block2: ROT'=ROT_firstvsROT_second, 'Block1: ROT vs. Block3: ROT'=ROT_firstvsROT_last,
                       'Block1: ROT vs. MIR'=ROT_firstvsMIR_first, 'Block2: ROT vs. MIR'=ROT_secondvsMIR_second, 'Block3: ROT vs. MIR'=ROT_lastvsMIR_last)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('perturbtype','block')), contrastList, adjust=method)
  
  print(comparisons)
}

#effect size
RTComparisonsAllBlocksEffSize <- function(group, method = 'bonferroni'){
  comparisons <- RTComparisonsAllBlocks(group=group,method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#So block 1 differs from aligned for ROT and MIR, but not washouts
#Last block differs from aligned for MIR, but not ROT
#ROT and MIR differ for blocks 1 and 2, but not block 3
#No differences across blocks within ROT, but there are for MIR

# Movement time analysis ----
# note
getMTTrials <- function(group, id, task, taskno){
  #allows for this function to work with each file
  #specify pp id, the task type, and task number
  #note that task type and taskno have to match, depending on present csv files

  
  dat <- getParticipantTaskData(group = group, id = id, taskno = taskno, task = task)
  trials <- unique(dat$trial) 
  proportion <- data.frame()
  
  for (trialno in trials){
    
    subndat <- dat[dat$trial == trialno,]
    subndat <- subndat[subndat$step == 4,]
    subndatsamp <- subndat[1,]
    
    if (nrow(subndat)==0){
      movementtime <- NA #will assign NA if step 4 does not occur
      trial <- trialno
      comment <- 'nostep' #this will help to keep track of how many trials did not have a step later on
      
    } else if(subndatsamp$trialselected_bool == 0){#we only want data that has been selected
      movementtime <- NA
      trial <- trialno
      comment <- 'unselected'
    } else{
      firststep4 <- subndat[1,]
      laststep4 <- subndat[nrow(subndat),]
      
      step4start <- firststep4$time_ms
      step4end <- laststep4$time_ms
      
      movementtime <- step4end - step4start
      trial <- trialno
      comment <- 'selected'
      
    }
    feedback <- c(trial, movementtime, task, comment)
    
    
    if (prod(dim(proportion)) == 0){
      proportion <- feedback
    } else {
      proportion <- rbind(proportion, feedback)
    }
  }
  proportion <- data.frame(proportion, row.names = NULL, stringsAsFactors = F)
  colnames(proportion) <- c('trial', 'movement_time', 'task', 'comment')
  proportion$participant <- id
  return(proportion)
}

getAlignedGroupMTTrials <- function(group = 'noninstructed', maxppid = 15) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppRT <- getMTTrials(group = group, id=participant, task = 'aligned', taskno = 1) #for every participant, get RT data
    
    reaction <- as.numeric(ppRT$movement_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    #task <- rep('AL', length(reaction))
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getMTAligned <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getAlignedGroupMTTrials(group=group, maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 1000)] <- NA #remove all greater than 1 second
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getMTBlockedAlignedConfInt <- function(group, maxppid, type = 't'){
  
  data <- getMTAligned(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 12 trials (they go through each of 12 possible targets in aligned)
  n <- 12;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/ALIGNED_noninstructed_blocked_CI_MT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ALIGNED_instructed_blocked_CI_MT.csv', row.names = F)
    } 
    
  }
  
}

#function below generates CIs for every trial
getMTAlignedConfInt <- function(group, maxppid, type = 't'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMTAligned(group = group, maxppid = maxppid)
  #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
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
      write.csv(confidence, file='data/ALIGNED_noninstructed_CI_MT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ALIGNED_instructed_CI_MT.csv', row.names = F)
    } 
    
  }
  
}

#Do the same for ROTATION

getROTGroupMTTrials <- function(group = 'noninstructed', maxppid = 15) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
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
      ppRT <- getMTTrials(group=group, id=participant, taskno = 11, task = 'rotation')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getMTTrials(group=group, id=participant, taskno = 5, task = 'rotation')
    }
    
    reaction <- as.numeric(ppRT$movement_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getMTRot <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getROTGroupMTTrials(group=group, maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 1000)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getMTBlockedROTConfInt <- function(group, maxppid, type = 't'){
  
  data <- getMTRot(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/ROT_noninstructed_blocked_CI_MT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROT_instructed_blocked_CI_MT.csv', row.names = F)
    } 
    
  }
  
}

#function below generates CIs for every trial
getMTROTConfInt <- function(group, maxppid, type = 't'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMTRot(group = group, maxppid = maxppid)
  #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
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
      write.csv(confidence, file='data/ROT_noninstructed_CI_MT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROT_instructed_CI_MT.csv', row.names = F)
    } 
    
  }
  
}

#Do the same for MIRROR

getMIRGroupMTTrials <- function(group = 'noninstructed', maxppid = 15) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
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
      ppRT <- getMTTrials(group=group, id=participant, taskno = 5, task = 'mirror')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getMTTrials(group=group, id=participant, taskno = 11, task = 'mirror')
    }
    
    reaction <- as.numeric(ppRT$movement_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getMTMir <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getMIRGroupMTTrials(group=group, maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 1000)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getMTBlockedMIRConfInt <- function(group, maxppid, type = 't'){
  
  data <- getMTMir(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/MIR_noninstructed_blocked_CI_MT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIR_instructed_blocked_CI_MT.csv', row.names = F)
    } 
    
  }
  
}

#function below generates CIs for every trial
getMTMIRConfInt <- function(group, maxppid, type = 't'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMTMir(group = group, maxppid = maxppid)
  #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
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
      write.csv(confidence, file='data/MIR_noninstructed_CI_MT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIR_instructed_CI_MT.csv', row.names = F)
    } 
    
  }
  
}


#Do the same for ROTWASH

getROTWASHGroupMTTrials <- function(group = 'noninstructed', maxppid = 15) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
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
      ppRT <- getMTTrials(group=group, id=participant, taskno = 13, task = 'washout1')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getMTTrials(group=group, id=participant, taskno = 7, task = 'washout0')
    }
    
    reaction <- as.numeric(ppRT$movement_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getMTRotwash <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getROTWASHGroupMTTrials(group=group, maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 1000)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getMTBlockedRotwashConfInt <- function(group, maxppid, type = 't'){
  
  data <- getMTRotwash(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/ROTWASH_noninstructed_blocked_CI_MT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROTWASH_instructed_blocked_CI_MT.csv', row.names = F)
    } 
    
  }
  
}

#function below generates CIs for every trial
getMTRotwashConfInt <- function(group, maxppid, type = 't'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMTRotwash(group = group, maxppid = maxppid)
  #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
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
      write.csv(confidence, file='data/ROTWASH_noninstructed_CI_MT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROTWASH_instructed_CI_MT.csv', row.names = F)
    } 
    
  }
  
}

#Do the same for MIRWASH

getMIRWASHGroupMTTrials <- function(group = 'noninstructed', maxppid = 15) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
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
      ppRT <- getMTTrials(group=group, id=participant, taskno = 7, task = 'washout0')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getMTTrials(group=group, id=participant, taskno = 13, task = 'washout1')
    }
    
    reaction <- as.numeric(ppRT$movement_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getMTMirwash <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getMIRWASHGroupMTTrials(group=group, maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 1000)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getMTBlockedMirwashConfInt <- function(group, maxppid, type = 't'){
  
  data <- getMTMirwash(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
      write.csv(confidence, file='data/MIRWASH_noninstructed_blocked_CI_MT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIRWASH_instructed_blocked_CI_MT.csv', row.names = F)
    } 
    
  }
  
}

#function below generates CIs for every trial
getMTMirwashConfInt <- function(group, maxppid, type = 't'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMTMirwash(group = group, maxppid = maxppid)
  #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
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
      write.csv(confidence, file='data/MIRWASH_noninstructed_CI_MT.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIRWASH_instructed_CI_MT.csv', row.names = F)
    } 
    
  }
  
}

plotNIBlockedMT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6_NI_movementtime.svg', width=11.5, height=8.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_noninstructed_blocked_CI_MT.csv')
  dat2 <- read.csv(file='data/ROT_noninstructed_blocked_CI_MT.csv')
  dat3 <- read.csv(file='data/ROTWASH_noninstructed_blocked_CI_MT.csv')
  dat4 <- read.csv(file='data/MIR_noninstructed_blocked_CI_MT.csv')
  dat5 <- read.csv(file='data/MIRWASH_noninstructed_blocked_CI_MT.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #separate each task, then plot as usual according to blocks
  
  X1 <- seq(1, 4,1)
  X3 <- seq(5,19,1)
  X5 <- seq(20,27,1)
  X7 <- seq(28,42,1)
  X9 <- seq(43,50,1)
  
  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Movement time (ms)', main = '',
       xlim = c(0,51), ylim = c(0,251))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,5,20,28,43,50))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(0, 100, 150, 200, 250),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(4.5,19.5,27.5,42.5), col = 8, lty = 2)
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:4], rev(YUp[1:4])), border=NA, col=alpha("#b4b4b4",.5))
  polygon(x = c(X3, rev(X3)), y = c(YLow[5:19], rev(YUp[5:19])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[20:27], rev(YUp[20:27])), border=NA, col="#e516362f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[28:42], rev(YUp[28:42])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[43:50], rev(YUp[43:50])), border=NA, col="#005de42f")
  
  lines(X1,Y[1:4], col = alpha("#000000", 1))#aligned
  lines(X3, Y[5:19], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[20:27], col = alpha("#e51636ff", 1))#rotwashout
  lines(X7, Y[28:42], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[43:50], col = alpha("#005de4ff", 1))#mirwashout
  

  
  #add legend
  legend(27,50,legend=c('Aligned','Rotation and washout','Mirror Reversal and washout'),
         col=c("#000000", "#e51636ff", "#005de4ff"),
         lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIBlockedMT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6_I_movementtime.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_instructed_blocked_CI_MT.csv')
  dat2 <- read.csv(file='data/ROT_instructed_blocked_CI_MT.csv')
  dat3 <- read.csv(file='data/ROTWASH_instructed_blocked_CI_MT.csv')
  dat4 <- read.csv(file='data/MIR_instructed_blocked_CI_MT.csv')
  dat5 <- read.csv(file='data/MIRWASH_instructed_blocked_CI_MT.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #separate each task, then plot as usual according to blocks
  
  X1 <- seq(1, 4,1)
  X3 <- seq(5,19,1)
  X5 <- seq(20,27,1)
  X7 <- seq(28,42,1)
  X9 <- seq(43,50,1)
  
  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Movement time (ms)', main = '',
       xlim = c(0,51), ylim = c(0,251))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,5,20,28,43,50))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(0, 100, 150, 200, 250),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(4.5,19.5,27.5,42.5), col = 8, lty = 2)
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:4], rev(YUp[1:4])), border=NA, col=alpha("#b4b4b4",.5))
  polygon(x = c(X3, rev(X3)), y = c(YLow[5:19], rev(YUp[5:19])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[20:27], rev(YUp[20:27])), border=NA, col="#e516362f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[28:42], rev(YUp[28:42])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[43:50], rev(YUp[43:50])), border=NA, col="#005de42f")
  
  lines(X1,Y[1:4], col = alpha("#000000", 1))#aligned
  lines(X3, Y[5:19], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[20:27], col = alpha("#e51636ff", 1))#rotwashout
  lines(X7, Y[28:42], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[43:50], col = alpha("#005de4ff", 1))#mirwashout
  
  
  
  #add legend
  legend(27,50,legend=c('Aligned','Rotation and washout','Mirror Reversal and washout'),
         col=c("#000000", "#e51636ff", "#005de4ff"),
         lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotBlockedMT <- function(target='inline'){
  #need to indicate non instructed and instructed in title
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6_movementtime.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(2,1))
  
  plotNIBlockedMT()
  plotIBlockedMT()
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Movement Time STATS----
#analysis is similar in structure to Reaction time, comparing aligned to ROT and MIR, but we also have washout here

getMTLongFormat <- function(groups = c('noninstructed','instructed'), maxppid = 15, location = 'maxvel'){
  
  for (group in groups){
    #Aligned data
    ALdat <- getMTAligned(group=group,maxppid=maxppid)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(ALdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    ALdat <- as.data.frame(ALdat)
    perturb <- rep('AL', nrow(ALdat))
    ALdat <- cbind(ALdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longALdata <- gather(ALdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longALdata, file=sprintf('data/ALIGNED_%s_MT_long.csv', group), row.names = F)
    
    
    #Rotation data
    ROTdat <- getMTRot(group=group,maxppid=maxppid)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(ROTdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    ROTdat <- as.data.frame(ROTdat)
    perturb <- rep('ROT', nrow(ROTdat))
    ROTdat <- cbind(ROTdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longROTdata <- gather(ROTdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longROTdata, file=sprintf('data/ROT_%s_MT_long.csv', group), row.names = F)
    
    #RotWASH data
    ROTWASHdat <- getMTRotwash(group=group,maxppid=maxppid)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(ROTWASHdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    ROTWASHdat <- as.data.frame(ROTWASHdat)
    perturb <- rep('ROTWASH', nrow(ROTWASHdat))
    ROTWASHdat <- cbind(ROTWASHdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longROTWASHdata <- gather(ROTWASHdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longROTWASHdata, file=sprintf('data/ROTWASH_%s_MT_long.csv', group), row.names = F)
    
    #Mirror data
    MIRdat <- getMTMir(group=group,maxppid=maxppid)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(MIRdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    MIRdat <- as.data.frame(MIRdat)
    perturb <- rep('MIR', nrow(MIRdat))
    MIRdat <- cbind(MIRdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longMIRdata <- gather(MIRdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longMIRdata, file=sprintf('data/MIR_%s_MT_long.csv', group), row.names = F)
    
    #MirWASH data
    MIRWASHdat <- getMTMirwash(group=group,maxppid=maxppid)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(MIRWASHdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    MIRWASHdat <- as.data.frame(MIRWASHdat)
    perturb <- rep('MIRWASH', nrow(MIRWASHdat))
    MIRWASHdat <- cbind(MIRWASHdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longMIRWASHdata <- gather(MIRWASHdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longMIRWASHdata, file=sprintf('data/MIRWASH_%s_MT_long.csv', group), row.names = F)
  }
}

#First, we grab data from aligned, since it is the baseline for all
getMTBlockedAlignedData <- function(group, blockdefs){
  LCaov <- data.frame()
  curves <- read.csv(sprintf('data/ALIGNED_%s_MT_long.csv',group), stringsAsFactors=FALSE)  
  participants <- unique(curves$participant)
  #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
  #curves <- curves[,-1] #take away trial column
  N <- length(participants) #gets the number of participants
  perturb = 'AL'
  #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
  #row.names(blocked) <- participants
  #colnames(blocked) <- names(blockdefs)
  
  perturbtype <- c()
  participant <- c()
  block <- c()
  compensation <- c()
  
  for (pp.idx in c(1:length(participants))) {
    
    pp <- participants[pp.idx] #loop through each participant
    
    for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
      
      blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
      blockstart <- blockdef[1] #either trial 1, 4, or 76
      blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
      #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
      # moved to long format files:
      samples <- c()
      for (trial in c(blockstart:blockend)) {
        # print(which(curves$participant == pp))
        # print(which(curves$participant == pp & curves$trial == trial))
        samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
        
      }
      #print(mean(samples, na.rm=TRUE))
      #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
      perturbtype <- c(perturbtype, perturb)
      participant <- c(participant, pp) #the participant
      block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
      compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
    }
    
  }
  
  GroupLCBlocked <- data.frame(perturbtype,participant,block,compensation)
  
  
  if (prod(dim(LCaov)) == 0){
    LCaov <- GroupLCBlocked
  } else {
    LCaov <- rbind(LCaov, GroupLCBlocked)
  }
  
  #need to make some columns as factors for ANOVA
  LCaov$perturbtype <- as.factor(LCaov$perturbtype)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
}

#Then grab data for washout and perturb types

getMTBlockedPerturbData <- function(perturbations = c('ROT','MIR'), group, blockdefs) {
  #function reads in aftereffects_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  #to include instructed group, just create another for loop here
  for (perturb in perturbations){  
    curves <- read.csv(sprintf('data/%s_%s_MT_long.csv',perturb,group), stringsAsFactors=FALSE)  
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    perturbtype <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        perturbtype <- c(perturbtype, perturb)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(perturbtype,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$perturbtype <- as.factor(LCaov$perturbtype)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}

getMTBlockedWashoutData <- function(perturbations = c('ROTWASH','MIRWASH'), group, blockdefs) {
  #function reads in aftereffects_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  #to include instructed group, just create another for loop here
  for (perturb in perturbations){  
    curves <- read.csv(sprintf('data/%s_%s_MT_long.csv',perturb,group), stringsAsFactors=FALSE)  
    participants <- unique(curves$participant)
    #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
    #curves <- curves[,-1] #take away trial column
    N <- length(participants) #gets the number of participants
    
    #blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
    #row.names(blocked) <- participants
    #colnames(blocked) <- names(blockdefs)
    
    perturbtype <- c()
    participant <- c()
    block <- c()
    compensation <- c()
    
    for (pp.idx in c(1:length(participants))) {
      
      pp <- participants[pp.idx] #loop through each participant
      
      for (blockno in c(1:length(blockdefs))) { #loop through each block (first, second, third)
        
        blockdef <- blockdefs[[blockno]] #creates a list which specifies start trial of every block, and how many trials in total for this block
        blockstart <- blockdef[1] #either trial 1, 4, or 76
        blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
        #samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
        # moved to long format files:
        samples <- c()
        for (trial in c(blockstart:blockend)) {
          # print(which(curves$participant == pp))
          # print(which(curves$participant == pp & curves$trial == trial))
          samples <- c(samples, curves$compensation[which(curves$participant == pp & curves$trial == trial)]) #get reachdev for current pp and trial
          
        }
        #print(mean(samples, na.rm=TRUE))
        #blocked[pp.idx,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
        perturbtype <- c(perturbtype, perturb)
        participant <- c(participant, pp) #the participant
        block <- c(block, names(blockdefs)[blockno]) #the name of the block number (first, second or third)
        compensation <- c(compensation, mean(samples, na.rm=T)) #mean compensation of trials for that block
      }
      
    }
    
    GroupLCBlocked <- data.frame(perturbtype,participant,block,compensation)
    
    
    if (prod(dim(LCaov)) == 0){
      LCaov <- GroupLCBlocked
    } else {
      LCaov <- rbind(LCaov, GroupLCBlocked)
    }
  }
  #need to make some columns as factors for ANOVA
  LCaov$perturbtype <- as.factor(LCaov$perturbtype)
  LCaov$block <- as.factor(LCaov$block)
  LCaov$block <- factor(LCaov$block, levels = c('first','second','last')) #so that it does not order it alphabetically
  return(LCaov)
  
}


#Note that blockdefs will differ depending on trial type
# Aligned: blockdefs <- list('first'=c(1,12),'second'=c(13,12),'last'=c(37,12))
# WASH:  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6))
# PERTURB:  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6))
# we compare all first blocks of perturb and washout types to last block of aligned

MTt.test <- function(group) {
  
  blockdefs <- list('first'=c(1,12),'second'=c(13,12),'last'=c(37,12))
  LC4test1 <- getMTBlockedAlignedData(group=group,blockdefs=blockdefs)
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  LC4test2 <- getMTBlockedPerturbData(group=group,blockdefs=blockdefs)
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6)) #6 trials per block
  LC4test3 <- getMTBlockedWashoutData(group=group,blockdefs=blockdefs)
  
  LC4test <- rbind(LC4test1, LC4test2, LC4test3)
  LC4test$participant <- as.factor(LC4test$participant)
  
  ALdat <- LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'AL'),]
  ROTdat <- LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'ROT'),]
  MIRdat <-LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'MIR'),]
  ROTWASHdat <-LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'ROTWASH'),]
  MIRWASHdat <-LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'MIRWASH'),]
  
  cat('Aligned (last block) compared to Rotation (first block):\n')
  print(t.test(ALdat$compensation, ROTdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, ROTdat$compensation))
  
  cat('Aligned (last block) compared to Mirror (first block):\n')
  print(t.test(ALdat$compensation, MIRdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, MIRdat$compensation))
  
  cat('Aligned (last block) compared to Rotation Washout (first block):\n')
  print(t.test(ALdat$compensation, ROTWASHdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, ROTWASHdat$compensation))
  
  cat('Aligned (last block) compared to Mirror Washout (first block):\n')
  print(t.test(ALdat$compensation, MIRWASHdat$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, MIRWASHdat$compensation))
  
  #I add a comparison of last block of ROT and MIR compared to aligned, and ROTWASH and MIRWASH last blocks.
  ROTdatlast <- LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'ROT'),]
  MIRdatlast <-LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'MIR'),]
  ROTWASHdatlast <-LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'ROTWASH'),]
  MIRWASHdatlast <-LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'MIRWASH'),]
  
  cat('Aligned (last block) compared to Rotation (last block):\n')
  print(t.test(ALdat$compensation, ROTdatlast$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, ROTdatlast$compensation))
  
  cat('Aligned (last block) compared to Mirror (last block):\n')
  print(t.test(ALdat$compensation, MIRdatlast$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, MIRdatlast$compensation))
  
  cat('Aligned (last block) compared to Rotation Washout (last block):\n')
  print(t.test(ALdat$compensation, ROTWASHdatlast$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, ROTWASHdatlast$compensation))
  
  cat('Aligned (last block) compared to Mirror Washout (last block):\n')
  print(t.test(ALdat$compensation, MIRWASHdatlast$compensation, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ALdat$compensation, MIRWASHdatlast$compensation))
  
}

# ROT and ROTWASH seem to be faster than ALIGNED.
# MIR starts of not different from aligned, but eventually becomes faster. MIRWASH does not differ from aligned.
# We'll compare ROT and WASH similarly to RT data, but we can also do the same for Washout data given these findings.

#One issue that arises is that ezANOVA needs complete cases for it to complete the analysis. P001, has a lot of deleted MIR MT trials.
# This results in MIR 2nd block to be a NaN. I remove this participant from analyses below.

movementtimePerturbANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- getMTBlockedPerturbData(group=group,blockdefs=blockdefs)
  
  if(group == 'noninstructed'){
    LC4aov <- LC4aov[-which(LC4aov$participant == 'p1'),]
  } else if (group == 'instructed'){
    LC4aov <- LC4aov[-which(LC4aov$participant == 'p13'),]
  }
  
  
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=c(perturbtype, block),type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

#No main effects, not interaction effects. Further analysis below is commented out as it is unnecessary.

MTPerturbComparisonMeans <- function(group){

  #can plot interaction just to eyeball it:
  #plot(interactionMeans(lm(compensation ~ block * perturbtype, data=LC4aov), factors=c('perturbtype', 'block'), atx='block'))

  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6))

  LC4aov <- getMTBlockedPerturbData(group=group, blockdefs=blockdefs)
  if(group == 'noninstructed'){
    LC4aov <- LC4aov[-which(LC4aov$participant == 'p1'),]
  } else if (group == 'instructed'){
    LC4aov <- LC4aov[-which(LC4aov$participant == 'p13'),]
  }
  secondAOV <- aov_ez("participant","compensation",LC4aov,within=c("perturbtype","block"))

  #nice(secondAOV, correction = 'none') #correction set to none since first AOV reveals no violation of sphericity
  #summary(secondAOV) #shows all results
  #run code above for figuring out df
  #output is the same
  #follow-ups using emmeans

  cellmeans <- emmeans(secondAOV,specs=c('perturbtype','block'))
  #cellmeans <- lsmeans(secondAOV$aov,specs=c('perturbtype','block'))
  print(cellmeans)
}

MTPerturbComparisonsAllBlocks <- function(group, method='bonferroni'){
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6))

  LC4aov <- getMTBlockedPerturbData(group=group,blockdefs=blockdefs)
  if(group == 'noninstructed'){
    LC4aov <- LC4aov[-which(LC4aov$participant == 'p1'),]
  } else if (group == 'instructed'){
    LC4aov <- LC4aov[-which(LC4aov$participant == 'p13'),]
  }
  secondAOV <- aov_ez("participant","compensation",LC4aov,within=c("perturbtype","block"))
  #based on cellmeans, confidence intervals and plots give us an idea of what contrasts we want to compare

  MIR_firstvsMIR_second  <- c(1,0,-1,0,0,0)
  MIR_firstvsMIR_last    <- c(1,0,0,0,-1,0)
  ROT_firstvsROT_second  <- c(0,1,0,-1,0,0)
  ROT_firstvsROT_last    <- c(0,1,0,0,0,-1)
  ROT_firstvsMIR_first   <- c(1,-1,0,0,0,0)
  ROT_secondvsMIR_second <- c(0,0,1,-1,0,0)
  ROT_lastvsMIR_last     <- c(0,0,0,0,1,-1)

  contrastList <- list('Block1: MIR vs. Block2: MIR'=MIR_firstvsMIR_second, 'Block1: MIR vs. Block3: MIR'=MIR_firstvsMIR_last,
                       'Block1: ROT vs. Block2: ROT'=ROT_firstvsROT_second, 'Block1: ROT vs. Block3: ROT'=ROT_firstvsROT_last,
                       'Block1: ROT vs. MIR'=ROT_firstvsMIR_first, 'Block2: ROT vs. MIR'=ROT_secondvsMIR_second, 'Block3: ROT vs. MIR'=ROT_lastvsMIR_last)

  comparisons<- contrast(emmeans(secondAOV,specs=c('perturbtype','block')), contrastList, adjust=method)

  print(comparisons)
}

#effect size
MTPerturbComparisonsAllBlocksEffSize <- function(group, method = 'bonferroni'){
  comparisons <- MTPerturbComparisonsAllBlocks(group=group,method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#washout data has complete data (include p001 in analysis; results do not differ whether or not we include this participant).
movementtimeWashoutANOVA <- function(group) {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6)) #6 trials per block
  
  LC4aov <- getMTBlockedWashoutData(group=group,blockdefs=blockdefs)
  #remove participant below?
  #LC4aov <- LC4aov[-which(LC4aov$participant == 'p1'),]
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=c(perturbtype, block),type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

#No main effects, not interaction effects. Further analysis below is commented out as it is unnecessary.

# MTWashoutComparisonMeans <- function(){
# 
#   #can plot interaction just to eyeball it:
#   plot(interactionMeans(lm(compensation ~ block * perturbtype, data=LC4aov), factors=c('perturbtype', 'block'), atx='block'))
# 
#   blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6))
# 
#   LC4aov <- getMTBlockedWashoutData(blockdefs=blockdefs)
#   #LC4aov <- LC4aov[-which(LC4aov$participant == 'p1'),]
#   secondAOV <- aov_ez("participant","compensation",LC4aov,within=c("perturbtype","block"))
# 
#   #nice(secondAOV, correction = 'none') #correction set to none since first AOV reveals no violation of sphericity
#   #summary(secondAOV) #shows all results
#   #run code above for figuring out df
#   #output is the same
#   #follow-ups using emmeans
# 
#   cellmeans <- emmeans(secondAOV,specs=c('perturbtype','block'))
#   #cellmeans <- lsmeans(secondAOV$aov,specs=c('perturbtype','block'))
#   print(cellmeans)
# }
# 
# MTWashoutComparisonsAllBlocks <- function(method='bonferroni'){
#   #styles <- getStyle()
#   blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6))
# 
#   LC4aov <- getMTBlockedWashoutData(blockdefs=blockdefs)
#   #LC4aov <- LC4aov[-which(LC4aov$participant == 'p1'),]
#   secondAOV <- aov_ez("participant","compensation",LC4aov,within=c("perturbtype","block"))
#   #based on cellmeans, confidence intervals and plots give us an idea of what contrasts we want to compare
# 
#   ROT_firstvsROT_second  <- c(1,0,-1,0,0,0)
#   ROT_firstvsROT_last    <- c(1,0,0,0,-1,0)
#   MIR_firstvsMIR_second  <- c(0,1,0,-1,0,0)
#   MIR_firstvsMIR_last    <- c(0,1,0,0,0,-1)
#   ROT_firstvsMIR_first   <- c(1,-1,0,0,0,0)
#   ROT_secondvsMIR_second <- c(0,0,1,-1,0,0)
#   ROT_lastvsMIR_last     <- c(0,0,0,0,1,-1)
# 
#   contrastList <- list('Block1: ROT vs. Block2: ROT'=ROT_firstvsROT_second, 'Block1: ROT vs. Block3: ROT'=ROT_firstvsROT_last,
#                        'Block1: MIR vs. Block2: MIR'=MIR_firstvsMIR_second, 'Block1: MIR vs. Block3: MIR'=MIR_firstvsMIR_last,
#                        'Block1: ROT vs. MIR'=ROT_firstvsMIR_first, 'Block2: ROT vs. MIR'=ROT_secondvsMIR_second, 'Block3: ROT vs. MIR'=ROT_lastvsMIR_last)
# 
#   comparisons<- contrast(emmeans(secondAOV$aov,specs=c('perturbtype','block')), contrastList, adjust=method)
# 
#   print(comparisons)
# }

#Based on t-tests:
# ROT and ROTWASH seem to be faster than ALIGNED.
# MIR starts of not different from aligned, but eventually becomes faster. MIRWASH does not differ from aligned.
# ROT and MIR were compared MTs were compared across blocks (1st, 2nd, last). Same for ROTWASH and MIRWASH.
# No significant main and interaction effects are found. So we can make interpretations based on the t-tests.
# MT seems to be partially affected by perturbation type (although evidence is weak), where rotation trials may be faster than baseline reaches.
# Mirror trials eventually get to the same speed as rotation trials, perhaps showing the engagement of strategy-based learning in
#mirror trials at the start.