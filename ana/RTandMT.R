#Reaction Time across Blocks -----

#movement time measures are originally found in testCorrect.R script - this has been updated here see MT analysis section
#code for RT will be similar, but now we only want step 3
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
  
  #only steps 6 and 7 will have 0 or 1 in trial_column
  #ndat <- dat[dat$step == 3, ]
  
  trials <- unique(dat$trial) #need to be dat, not ndat because not all participants have step 3 recorded on every trial
  proportion <- data.frame()
  
  for (trialno in trials){
    
    subndat <- dat[dat$trial == trialno,]
    subndat <- subndat[subndat$step == 3,]
    
    if (nrow(subndat)==0){
      reactiontime <- NA #will assign NaN if step 3 does not occur
      trial <- trialno
      
    } else{
      firststep3 <- subndat[1,]
      laststep3 <- subndat[nrow(subndat),]
      
      step3start <- firststep3$time_ms
      step3end <- laststep3$time_ms
      
      reactiontime <- step3end - step3start
      trial <- trialno
      
    }
    feedback <- c(trial, reactiontime, task)
    
    
    if (prod(dim(proportion)) == 0){
      proportion <- feedback
    } else {
      proportion <- rbind(proportion, feedback)
    }
  }
  proportion <- data.frame(proportion, row.names = NULL, stringsAsFactors = F)
  colnames(proportion) <- c('trial', 'reaction_time', 'task')
  proportion$participant <- id
  return(proportion)
}

getBlockedRTTrials <- function(group,id,task,taskno){
  
  data <- getRTTrials(group=group,id=id,task=task,taskno=taskno)
  reactiontime <- data.frame(as.numeric(data$reaction_time))
  
  #get mean of every 6th trial
  n <- 6
  ndat <- aggregate(reactiontime,list(rep(1:(nrow(reactiontime)%/%n+1),each=n,len=nrow(reactiontime))),mean, na.rm=TRUE)[-1]
  colnames(ndat) <- 'reaction_time'
  ndat$block <- seq(1,length(ndat$reaction_time),1)
  ndat$participant <- id
  ndat$task <- task
  
  return(ndat)
}


#but to actually do something with RT data, we could plot RT across trials separately for all MIR and ROT
#we would want all baseline, perturb, washout (aligned RTs and perturb RTs and washout)
#may have to create functions similar to Learning Curve data that does it for Aligned, Perturb, Washout


getAlignedRTData <- function(group, maxppid, task = 'aligned', taskno = 1){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  dataoutput<- data.frame()
  
  for(ppno in participants){
    dat <- getBlockedRTTrials(group=group, id=ppno, task=task, taskno=taskno)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- rbind(dataoutput, dat)
    }
  }
  #dataoutput <- subset(dataoutput, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(dataoutput)
}

getAlignedRTConfInt <- function(group, maxppid, task = 'aligned', taskno = 1, type = 't'){
  
  
  dat <- getAlignedRTData(group=group, maxppid=maxppid, task=task, taskno=taskno)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    reactiontime <- as.numeric(cireaches$reaction_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = reactiontime, variance = var(reactiontime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/ALIGNED_NI_CI_ReactionTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/ALIGNED_I_CI_ReactionTime.csv', row.names = F)
  }
}

getRotationRTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Rotdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      RotRT <- getBlockedRTTrials(group=group, id=pp, taskno = 11, task = 'rotation')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      RotRT <- getBlockedRTTrials(group=group, id=pp, taskno = 5, task = 'rotation')
    }
    
    if (prod(dim(Rotdat)) == 0){
      Rotdat <- RotRT
    } else {
      Rotdat <- rbind(Rotdat, RotRT)
    }
    
  }
  #Rotdat <- subset(Rotdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Rotdat)
}

getRotationRTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getRotationRTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    reactiontime <- as.numeric(cireaches$reaction_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = reactiontime, variance = var(reactiontime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/ROTATION_NI_CI_ReactionTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/ROTATION_I_CI_ReactionTime.csv', row.names = F)
  }
}

getMirrorRTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Mirdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      MirRT <- getBlockedRTTrials(group=group, id=pp, taskno = 5, task = 'mirror')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      MirRT <- getBlockedRTTrials(group=group, id=pp, taskno = 11, task = 'mirror')
    }
    
    if (prod(dim(Mirdat)) == 0){
      Mirdat <- MirRT
    } else {
      Mirdat <- rbind(Mirdat, MirRT)
    }
    
  }
  #Mirdat <- subset(Mirdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Mirdat)
}

getMirrorRTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getMirrorRTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    reactiontime <- as.numeric(cireaches$reaction_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = reactiontime, variance = var(reactiontime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/MIRROR_NI_CI_ReactionTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/MIRROR_I_CI_ReactionTime.csv', row.names = F)
  }
  
}

getRotationWashRTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Rotdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      RotRT <- getBlockedRTTrials(group=group, id=pp, taskno = 13, task = 'washout1')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      RotRT <- getBlockedRTTrials(group=group, id=pp, taskno = 7, task = 'washout0')
    }
    
    if (prod(dim(Rotdat)) == 0){
      Rotdat <- RotRT
    } else {
      Rotdat <- rbind(Rotdat, RotRT)
    }
    
  }
  #Rotdat <- subset(Rotdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Rotdat)
}

getRotationWashRTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getRotationWashRTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    reactiontime <- as.numeric(cireaches$reaction_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = reactiontime, variance = var(reactiontime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/ROTATIONWASH_NI_CI_ReactionTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/ROTATIONWASH_I_CI_ReactionTime.csv', row.names = F)
  }
  
}

getMirrorWashRTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Mirdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      MirRT <- getBlockedRTTrials(group=group, id=pp, taskno = 7, task = 'washout0')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      MirRT <- getBlockedRTTrials(group=group, id=pp, taskno = 13, task = 'washout1')
    }
    
    if (prod(dim(Mirdat)) == 0){
      Mirdat <- MirRT
    } else {
      Mirdat <- rbind(Mirdat, MirRT)
    }
    
  }
  #Mirdat <- subset(Mirdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Mirdat)
}

getMirrorWashRTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getMirrorWashRTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    reactiontime <- as.numeric(cireaches$reaction_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = reactiontime, variance = var(reactiontime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/MIRRORWASH_NI_CI_ReactionTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/MIRRORWASH_I_CI_ReactionTime.csv', row.names = F)
  }
}

plotNIBlockedRT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6NI_reactiontime.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_NI_CI_ReactionTime.csv')
  dat2 <- read.csv(file='data/ROTATION_NI_CI_ReactionTime.csv')
  dat3 <- read.csv(file='data/ROTATIONWASH_NI_CI_ReactionTime.csv')
  dat4 <- read.csv(file='data/MIRROR_NI_CI_ReactionTime.csv')
  dat5 <- read.csv(file='data/MIRRORWASH_NI_CI_ReactionTime.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #should be the same idea as points in movement time, so may need to separate each task, then plot as usual
  
  X1 <- seq(1, 8,1)
  X3 <- seq(9,23,1)
  X5 <- seq(24,31,1)
  X7 <- seq(32,46,1)
  X9 <- seq(47,54,1)
  
  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Reaction Time (ms)', main = 'Mean Reaction Time across Blocks and Tasks',
       xlim = c(0,55), ylim = c(0,1000))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,9,24,32,47,54))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(0, 200, 300, 400,500, 600, 700, 800, 1000),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(8.5,23.5,31.5,46.5), col = 8, lty = 2)
  
  #localization
  lines(X1,Y[1:8], col = alpha("#696969ff", 1))#aligned
  lines(X3, Y[9:23], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[24:31], col = alpha("#ff8200ff", 1))#rotwashout
  lines(X7, Y[32:46], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[47:54], col = alpha("#c400c4ff", 1))#mirwashout
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:8], rev(YUp[1:8])), border=NA, col="#6969692f")
  polygon(x = c(X3, rev(X3)), y = c(YLow[9:23], rev(YUp[9:23])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[24:31], rev(YUp[24:31])), border=NA, col="#ff82002f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[32:46], rev(YUp[32:46])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[47:54], rev(YUp[47:54])), border=NA, col="#c400c42f")
  
  #add legend
  legend(48,1000,legend=c('Aligned','Rotation','Washout: ROT','Mirror Reversal','Washout: MIR'),
         col=c("#696969ff", "#e51636ff", "#ff8200ff", "#005de4ff", "#c400c4ff"),
         lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIBlockedRT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6I_reactiontime.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_I_CI_ReactionTime.csv')
  dat2 <- read.csv(file='data/ROTATION_I_CI_ReactionTime.csv')
  dat3 <- read.csv(file='data/ROTATIONWASH_I_CI_ReactionTime.csv')
  dat4 <- read.csv(file='data/MIRROR_I_CI_ReactionTime.csv')
  dat5 <- read.csv(file='data/MIRRORWASH_I_CI_ReactionTime.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #should be the same idea as points in movement time, so may need to separate each task, then plot as usual
  
  X1 <- seq(1, 8,1)
  X3 <- seq(9,23,1)
  X5 <- seq(24,31,1)
  X7 <- seq(32,46,1)
  X9 <- seq(47,54,1)
  
  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Reaction Time (ms)', main = 'Mean Reaction Time across Blocks and Tasks',
       xlim = c(0,55), ylim = c(0,1000))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,9,24,32,47,54))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(0, 200, 300, 400,500, 600, 700, 800, 1000),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(8.5,23.5,31.5,46.5), col = 8, lty = 2)
  
  #localization
  lines(X1,Y[1:8], col = alpha("#696969ff", 1))#aligned
  lines(X3, Y[9:23], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[24:31], col = alpha("#ff8200ff", 1))#rotwashout
  lines(X7, Y[32:46], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[47:54], col = alpha("#c400c4ff", 1))#mirwashout
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:8], rev(YUp[1:8])), border=NA, col="#6969692f")
  polygon(x = c(X3, rev(X3)), y = c(YLow[9:23], rev(YUp[9:23])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[24:31], rev(YUp[24:31])), border=NA, col="#ff82002f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[32:46], rev(YUp[32:46])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[47:54], rev(YUp[47:54])), border=NA, col="#c400c42f")
  
  #add legend
  legend(48,1000,legend=c('Aligned','Rotation','Washout: ROT','Mirror Reversal','Washout: MIR'),
         col=c("#696969ff", "#e51636ff", "#ff8200ff", "#005de4ff", "#c400c4ff"),
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
    svglite(file='doc/fig/Fig6_reactiontime.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(2,1))
  
  plotNIBlockedRT()
  plotIBlockedRT()
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# #then we use the function above and generate it for all tasks
# getRTTasks <- function(id){
#   
#   #if pp id is odd
#   #mirror is first than rotation
#   if (id%%2 == 1){
#     
#     out1 <- getRTTrials(id = id, task = 'aligned', taskno = 1)
#     out2 <- getRTTrials(id = id, task = 'random0', taskno = 3)
#     out3 <- getRTTrials(id = id, task = 'mirror', taskno = 5)
#     out4 <- getRTTrials(id = id, task = 'washout0', taskno = 7)
#     out5 <- getRTTrials(id = id, task = 'random1', taskno = 9)
#     out6 <- getRTTrials(id = id, task = 'rotation', taskno = 11)
#     out7 <- getRTTrials(id = id, task = 'washout1', taskno = 13)
#     
#     allout <- rbind(out1,out2,out3,out4,out5,out6,out7)
#     allout$participant <- id
#     
#   } else if (id%%2 == 0){
#     #if pp id is even
#     #rotation first then mirror
#     out1 <- getRTTrials(id = id, task = 'aligned', taskno = 1)
#     out2 <- getRTTrials(id = id, task = 'random0', taskno = 3)
#     out3 <- getRTTrials(id = id, task = 'rotation', taskno = 5)
#     out4 <- getRTTrials(id = id, task = 'washout0', taskno = 7)
#     out5 <- getRTTrials(id = id, task = 'random1', taskno = 9)
#     out6 <- getRTTrials(id = id, task = 'mirror', taskno = 11)
#     out7 <- getRTTrials(id = id, task = 'washout1', taskno = 13)
#     
#     allout <- rbind(out1,out2,out3,out4,out5,out6,out7)
#     allout$participant <- id
#   }
#   return(allout)
# }
# 
# #then we can plot every RT for every trial, across all tasks
# plotParticipantRT <- function(id){
#   
#   df <- getRTTasks(id=id)
#   full_trial <- seq(1,length(df$reaction_time),1)
#   df <- cbind(df,full_trial)
#   #yupperlim <- max(as.numeric(df$movement_time))
#   
#   X1 <- seq(1, 48,1)
#   X3 <- seq(49,96,1)
#   X5 <- seq(97,186,1)
#   X7 <- seq(187,234,1)
#   X9 <- seq(235,282,1)
#   X11 <- seq(283,372,1)
#   X13 <- seq(373,420,1)
#   
#   Y <- as.numeric(df$reaction_time)
#   
#   plot(c(1:length(df$full_trial)), Y, type = 'n', axes = FALSE,
#        xlab = '', ylab = '', main = sprintf('pp %d: Reaction Time (ms) across Trials & Tasks', id),
#        xlim = c(0,420), ylim = c(0,2000))
#   
#   #localization
#   points(X1,Y[1:48], col = alpha("#e51636ff", 0.5))#aligned
#   points(X3, Y[49:96], col = alpha("#c400c4ff", 0.5))#random0
#   points(X5, Y[97:186], col = alpha("#005de4ff", 0.5))#perturb0
#   points(X7, Y[187:234], col = alpha("#e51636ff", 0.5))#washout0
#   points(X9, Y[235:282], col = alpha("#c400c4ff", 0.5))#random1
#   points(X11, Y[283:372], col = alpha("#005de4ff", 0.5))#perturb1
#   points(X13, Y[373:420], col = alpha("#e51636ff",0.5))#washout1
#   
#   if (id%%2 == 1){
#     
#     labs <- c('1:AL','49:RDM0','97:MIR','187:WASH0','235:RDM1','283:ROT','373:WASH1','420')
#     
#   } else if (id%%2 == 0){
#     #if pp id is even
#     #rotation first then mirror
#     labs <- c('1:AL','49:RDM0','97:ROT','187:WASH0','235:RDM1','283:MIR','373:WASH1','420')
#   }
#   
#   
#   axis(side=1, at=c(1,49,97,187,235,283,373,420), labels=labs)
#   #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
#   axis(side=2, at=c(0,400,700,1000,2000),las=2)
#   
#   abline(h = c(400,700), col = 'black', lty = 2)
#   abline(v = c(49,97,187,235,283,373), col = 8, lty = 1)
#   
# }
# 
# plotAllRT <- function(maxppno, target='inline'){
#   
#   if (target=='svg') {
#     svglite(file='doc/fig/Fig1_MT.svg', width=18, height=10, pointsize=14, system_fonts=list(sans="Arial"))
#   }
#   
#   ppno <- seq(0,maxppno,1)
#   par(mfrow=c(6,2),mar=c(3,3,3,3))
#   
#   for (pp in ppno){
#     plotParticipantRT(id=pp)
#   }
#   #mtext('Movement Time (ms)', side=2, outer=TRUE, line=-1, cex=1)
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
# }



# Movement time analysis ----


getBlockedMTTrials <- function(group,id,task,taskno){
  
  data <- getMTTrials(group=group,id=id,task=task,taskno=taskno)
  movementtime <- data.frame(as.numeric(data$movement_time))
  
  #get mean of every 6th trial
  n <- 6
  ndat <- aggregate(movementtime,list(rep(1:(nrow(movementtime)%/%n+1),each=n,len=nrow(movementtime))),mean, na.rm=TRUE)[-1]
  colnames(ndat) <- 'movement_time'
  ndat$block <- seq(1,length(ndat$movement_time),1)
  ndat$participant <- id
  ndat$task <- task
  
  return(ndat)
}


#but to actually do something with RT data, we could plot RT across trials separately for all MIR and ROT
#we would want all baseline, perturb, washout (aligned RTs and perturb RTs and washout)
#may have to create functions similar to Learning Curve data that does it for Aligned, Perturb, Washout


getAlignedMTData <- function(group, maxppid, task = 'aligned', taskno = 1){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  dataoutput<- data.frame()
  
  for(ppno in participants){
    dat <- getBlockedMTTrials(group=group, id=ppno, task=task, taskno=taskno)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- rbind(dataoutput, dat)
    }
  }
  #dataoutput <- subset(dataoutput, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(dataoutput)
}

getAlignedMTConfInt <- function(group, maxppid, task = 'aligned', taskno = 1, type = 't'){
  
  
  dat <- getAlignedMTData(group=group, maxppid=maxppid, task=task, taskno=taskno)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    movementtime <- as.numeric(cireaches$movement_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = movementtime, variance = var(movementtime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/ALIGNED_NI_CI_MovementTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/ALIGNED_I_CI_MovementTime.csv', row.names = F)
  }
}

getRotationMTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Rotdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      RotMT <- getBlockedMTTrials(group=group, id=pp, taskno = 11, task = 'rotation')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      RotMT <- getBlockedMTTrials(group=group, id=pp, taskno = 5, task = 'rotation')
    }
    
    if (prod(dim(Rotdat)) == 0){
      Rotdat <- RotMT
    } else {
      Rotdat <- rbind(Rotdat, RotMT)
    }
    
  }
  #Rotdat <- subset(Rotdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Rotdat)
}

getRotationMTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getRotationMTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    movementtime <- as.numeric(cireaches$movement_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = movementtime, variance = var(movementtime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/ROTATION_NI_CI_MovementTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/ROTATION_I_CI_MovementTime.csv', row.names = F)
  }
}

getMirrorMTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Mirdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      MirMT <- getBlockedMTTrials(group=group, id=pp, taskno = 5, task = 'mirror')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      MirMT <- getBlockedMTTrials(group=group, id=pp, taskno = 11, task = 'mirror')
    }
    
    if (prod(dim(Mirdat)) == 0){
      Mirdat <- MirMT
    } else {
      Mirdat <- rbind(Mirdat, MirMT)
    }
    
  }
  #Mirdat <- subset(Mirdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Mirdat)
}

getMirrorMTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getMirrorMTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    movementtime <- as.numeric(cireaches$movement_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = movementtime, variance = var(movementtime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/MIRROR_NI_CI_MovementTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/MIRROR_I_CI_MovementTime.csv', row.names = F)
  }
  
}

getRotationWashMTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Rotdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      RotMT <- getBlockedMTTrials(group=group, id=pp, taskno = 13, task = 'washout1')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      RotMT <- getBlockedMTTrials(group=group, id=pp, taskno = 7, task = 'washout0')
    }
    
    if (prod(dim(Rotdat)) == 0){
      Rotdat <- RotMT
    } else {
      Rotdat <- rbind(Rotdat, RotMT)
    }
    
  }
  #Rotdat <- subset(Rotdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Rotdat)
}

getRotationWashMTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getRotationWashMTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    movementtime <- as.numeric(cireaches$movement_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = movementtime, variance = var(movementtime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/ROTATIONWASH_NI_CI_MovementTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/ROTATIONWASH_I_CI_MovementTime.csv', row.names = F)
  }
  
}

getMirrorWashMTData <- function(group, maxppid){
  
  #a consequence of adding the groups late led me to fix it in the manner below
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  #participants <- seq(0,maxppid,1)
  Mirdat <- data.frame()
  
  for(pp in participants){
    if (pp%%2 == 1){
      #mirror then rotation if odd id
      MirMT <- getBlockedMTTrials(group=group, id=pp, taskno = 7, task = 'washout0')
    } else if (pp%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      MirMT <- getBlockedMTTrials(group=group, id=pp, taskno = 13, task = 'washout1')
    }
    
    if (prod(dim(Mirdat)) == 0){
      Mirdat <- MirMT
    } else {
      Mirdat <- rbind(Mirdat, MirMT)
    }
    
  }
  #Mirdat <- subset(Mirdat, participant != 4) #REMOVE FAULTY PARTICIPANT
  return(Mirdat)
}

getMirrorWashMTConfInt <- function(group, maxppid, type = 't'){
  
  
  dat <- getMirrorWashMTData(group=group, maxppid=maxppid)
  blocks <- unique(dat$block)
  confidence <- data.frame()
  
  
  for (blockno in blocks){
    cireaches <- dat[which(dat$block == blockno), ]
    movementtime <- as.numeric(cireaches$movement_time)
    
    if (type == "t"){
      #cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = movementtime, variance = var(movementtime), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  #return(confidence)
  if (group == 'noninstructed'){
    write.csv(confidence, file='data/MIRRORWASH_NI_CI_MovementTime.csv', row.names = F) 
  } else if (group == 'instructed'){
    write.csv(confidence, file='data/MIRRORWASH_I_CI_MovementTime.csv', row.names = F)
  }
}

plotNIBlockedMT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6NI_movementtime.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_NI_CI_MovementTime.csv')
  dat2 <- read.csv(file='data/ROTATION_NI_CI_MovementTime.csv')
  dat3 <- read.csv(file='data/ROTATIONWASH_NI_CI_MovementTime.csv')
  dat4 <- read.csv(file='data/MIRROR_NI_CI_MovementTime.csv')
  dat5 <- read.csv(file='data/MIRRORWASH_NI_CI_MovementTime.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #should be the same idea as points in movement time, so may need to separate each task, then plot as usual
  
  X1 <- seq(1, 8,1)
  X3 <- seq(9,23,1)
  X5 <- seq(24,31,1)
  X7 <- seq(32,46,1)
  X9 <- seq(47,54,1)
  
  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Movement Time (ms)', main = 'Mean Movement Time across Blocks and Tasks',
       xlim = c(0,55), ylim = c(0,1000))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,9,24,32,47,54))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(0, 200, 300, 400,500, 600, 700, 800, 1000),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(8.5,23.5,31.5,46.5), col = 8, lty = 2)
  
  #localization
  lines(X1,Y[1:8], col = alpha("#696969ff", 1))#aligned
  lines(X3, Y[9:23], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[24:31], col = alpha("#ff8200ff", 1))#rotwashout
  lines(X7, Y[32:46], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[47:54], col = alpha("#c400c4ff", 1))#mirwashout
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:8], rev(YUp[1:8])), border=NA, col="#6969692f")
  polygon(x = c(X3, rev(X3)), y = c(YLow[9:23], rev(YUp[9:23])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[24:31], rev(YUp[24:31])), border=NA, col="#ff82002f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[32:46], rev(YUp[32:46])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[47:54], rev(YUp[47:54])), border=NA, col="#c400c42f")
  
  #add legend
  legend(48,1000,legend=c('Aligned','Rotation','Washout: ROT','Mirror Reversal','Washout: MIR'),
         col=c("#696969ff", "#e51636ff", "#ff8200ff", "#005de4ff", "#c400c4ff"),
         lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIBlockedMT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6I_movementtime.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_I_CI_MovementTime.csv')
  dat2 <- read.csv(file='data/ROTATION_I_CI_MovementTime.csv')
  dat3 <- read.csv(file='data/ROTATIONWASH_I_CI_MovementTime.csv')
  dat4 <- read.csv(file='data/MIRROR_I_CI_MovementTime.csv')
  dat5 <- read.csv(file='data/MIRRORWASH_I_CI_MovementTime.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #should be the same idea as points in movement time, so may need to separate each task, then plot as usual
  
  X1 <- seq(1, 8,1)
  X3 <- seq(9,23,1)
  X5 <- seq(24,31,1)
  X7 <- seq(32,46,1)
  X9 <- seq(47,54,1)
  
  Y <- as.numeric(dat$V2)
  YLow <- as.numeric(dat$V1)
  YUp <- as.numeric(dat$V3)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Blocks', ylab = 'Movement Time (ms)', main = 'Mean Movement Time across Blocks and Tasks',
       xlim = c(0,55), ylim = c(0,1000))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,9,24,32,47,54))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(0, 200, 300, 400,500, 600, 700, 800, 1000),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(8.5,23.5,31.5,46.5), col = 8, lty = 2)
  
  #localization
  lines(X1,Y[1:8], col = alpha("#696969ff", 1))#aligned
  lines(X3, Y[9:23], col = alpha("#e51636ff", 1))#rotation
  lines(X5, Y[24:31], col = alpha("#ff8200ff", 1))#rotwashout
  lines(X7, Y[32:46], col = alpha("#005de4ff", 1))#mirror
  lines(X9, Y[47:54], col = alpha("#c400c4ff", 1))#mirwashout
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:8], rev(YUp[1:8])), border=NA, col="#6969692f")
  polygon(x = c(X3, rev(X3)), y = c(YLow[9:23], rev(YUp[9:23])), border=NA, col="#e516362f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[24:31], rev(YUp[24:31])), border=NA, col="#ff82002f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[32:46], rev(YUp[32:46])), border=NA, col="#005de42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[47:54], rev(YUp[47:54])), border=NA, col="#c400c42f")
  
  #add legend
  legend(48,1000,legend=c('Aligned','Rotation','Washout: ROT','Mirror Reversal','Washout: MIR'),
         col=c("#696969ff", "#e51636ff", "#ff8200ff", "#005de4ff", "#c400c4ff"),
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

