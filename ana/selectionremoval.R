source('ana/shared.R')

#Trial Count----
countAlignedParticipantDeletedTrials<- function (groups=c('noninstructed','instructed'), task='aligned') {
  #aligned, rotation, rotwash, mirror, mirwash
  #participants are 0 to 15 for noninstructed
  #go through each pp data, and grabe unselected trials
  #divide by total trials to get a percentage
  delsum <- data.frame()
  for (group in groups){
    if(group == 'noninstructed'){
      pplist <- seq(0,15,1)
    } else if (group == 'instructed'){
      pplist <- seq(16,31,1)
    }
    for (pp in pplist){
      
      data <- getParticipantTaskData(group = group , id = pp, taskno = 1, task = task)
      
      total <- length(unique(data$trial))
      
      subdf <- data[which(data$trialselected_bool == 0),]
      deleted <- length(unique(subdf$trial))
      percentdeleted<- (deleted/total)*100
      del_trial <- data.frame(group, deleted, total, percentdeleted)
      tasktype <- rep('aligned', nrow(del_trial))
      del_trial <- cbind(del_trial, tasktype)
      colnames(del_trial) <- c('group','deleted_trials', 'total_trials', 'percent_deleted', 'task')
      
      if (prod(dim(delsum)) == 0){
        delsum <- del_trial
        row.names(delsum) <- NULL
      } else {
        delsum <- rbind(delsum, del_trial)
        row.names(delsum) <- NULL
      }
      
    }
    
  }
  
  return(delsum)
  #write.csv(delsum, file=sprintf('data/%s_%s_deletedtrials.csv', session, task),quote=FALSE,row.names=FALSE)
}

countRotatedParticipantDeletedTrials <- function(groups=c('noninstructed','instructed'), task='rotation'){
  
  delsum <- data.frame()
  delsum2<- data.frame()
  for (group in groups){
    if(group == 'noninstructed'){
      pplist <- seq(0,15,1)
    } else if (group == 'instructed'){
      pplist <- seq(16,31,1)
    }
    for (pp in pplist){
      if (pp%%2 == 1){
        #mirror then rotation if odd id
        ROTdata <- getParticipantTaskData(group=group, id=pp, taskno = 11, task = task)
        WASHROTdata <- getParticipantTaskData(group=group, id=pp, taskno = 13, task = 'washout1')
      } else if (pp%%2 == 0){
        #if pp id is even
        #rotation first then mirror
        ROTdata <- getParticipantTaskData(group=group, id=pp, taskno = 5, task = task)
        WASHROTdata <- getParticipantTaskData(group=group, id=pp, taskno = 7, task = 'washout0')
      }
      total <- length(unique(ROTdata$trial))
      
      subdf <- ROTdata[which(ROTdata$trialselected_bool == 0),]
      deleted <- length(unique(subdf$trial))
      percentdeleted<- (deleted/total)*100
      del_trial <- data.frame(group, deleted, total, percentdeleted)
      tasktype <- rep('rot', nrow(del_trial))
      del_trial <- cbind(del_trial, tasktype)
      colnames(del_trial) <- c('group','deleted_trials', 'total_trials', 'percent_deleted', 'task')
      
      if (prod(dim(delsum)) == 0){
        delsum <- del_trial
        row.names(delsum) <- NULL
      } else {
        delsum <- rbind(delsum, del_trial)
        row.names(delsum) <- NULL
      }
      
      total2 <- length(unique(WASHROTdata$trial))
      
      subdf2 <- WASHROTdata[which(WASHROTdata$trialselected_bool == 0),]
      deleted2 <- length(unique(subdf2$trial))
      percentdeleted2<- (deleted2/total2)*100
      del_trial2 <- data.frame(group, deleted2, total2, percentdeleted2)
      tasktype2 <- rep('washrot', nrow(del_trial))
      del_trial2 <- cbind(del_trial2, tasktype2)
      colnames(del_trial2) <- c('group','deleted_trials', 'total_trials', 'percent_deleted', 'task')
      
      if (prod(dim(delsum2)) == 0){
        delsum2 <- del_trial2
        row.names(delsum2) <- NULL
      } else {
        delsum2 <- rbind(delsum2, del_trial2)
        row.names(delsum2) <- NULL
      }
      
    }
    
  }
  delsumtot <- rbind(delsum,delsum2)
  return(delsumtot)
}

countMirroredParticipantDeletedTrials <- function(groups=c('noninstructed','instructed'), task='mirror'){
  
  delsum <- data.frame()
  delsum2<- data.frame()
  for (group in groups){
    if(group == 'noninstructed'){
      pplist <- seq(0,15,1)
    } else if (group == 'instructed'){
      pplist <- seq(16,31,1)
    }
    for (pp in pplist){
      if (pp%%2 == 1){
        #mirror then rotation if odd id
        MIRdata <- getParticipantTaskData(group=group, id=pp, taskno = 5, task = task)
        WASHMIRdata <- getParticipantTaskData(group=group, id=pp, taskno = 7, task = 'washout0')
      } else if (pp%%2 == 0){
        #if pp id is even
        #rotation first then mirror
        MIRdata <- getParticipantTaskData(group=group, id=pp, taskno = 11, task = task)
        WASHMIRdata <- getParticipantTaskData(group=group, id=pp, taskno = 13, task = 'washout1')
      }
      total <- length(unique(MIRdata$trial))
      
      subdf <- MIRdata[which(MIRdata$trialselected_bool == 0),]
      deleted <- length(unique(subdf$trial))
      percentdeleted<- (deleted/total)*100
      del_trial <- data.frame(group, deleted, total, percentdeleted)
      tasktype <- rep('mir', nrow(del_trial))
      del_trial <- cbind(del_trial, tasktype)
      colnames(del_trial) <- c('group','deleted_trials', 'total_trials', 'percent_deleted', 'task')
      
      if (prod(dim(delsum)) == 0){
        delsum <- del_trial
        row.names(delsum) <- NULL
      } else {
        delsum <- rbind(delsum, del_trial)
        row.names(delsum) <- NULL
      }
      
      total2 <- length(unique(WASHMIRdata$trial))
      
      subdf2 <- WASHMIRdata[which(WASHMIRdata$trialselected_bool == 0),]
      deleted2 <- length(unique(subdf2$trial))
      percentdeleted2<- (deleted2/total2)*100
      del_trial2 <- data.frame(group, deleted2, total2, percentdeleted2)
      tasktype2 <- rep('washmir', nrow(del_trial))
      del_trial2 <- cbind(del_trial2, tasktype2)
      colnames(del_trial2) <- c('group','deleted_trials', 'total_trials', 'percent_deleted', 'task')
      
      if (prod(dim(delsum2)) == 0){
        delsum2 <- del_trial2
        row.names(delsum2) <- NULL
      } else {
        delsum2 <- rbind(delsum2, del_trial2)
        row.names(delsum2) <- NULL
      }
      
    }
    
  }
  delsumtot <- rbind(delsum,delsum2)
  return(delsumtot)
}

getNIDeletedTrials <- function(group = 'noninstructed'){
  
  dat1 <- countAlignedParticipantDeletedTrials()
  dat2 <- countRotatedParticipantDeletedTrials()
  dat3 <- countMirroredParticipantDeletedTrials()
  
  alldat <- rbind(dat1,dat2,dat3)
  alldat <- alldat[which(alldat$group == group), ]
  
  cat('Aligned:\n')
  subdat <- alldat[which(alldat$task == 'aligned'), ]
  deleted <- sum(subdat$deleted_trials)
  total <- sum(subdat$total_trials)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
  
  cat('Rotated:\n')
  subdat <- alldat[which(alldat$task == 'rot'), ]
  deleted <- sum(subdat$deleted_trials)
  total <- sum(subdat$total_trials)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
  
  cat('Washout_Rotated:\n')
  subdat <- alldat[which(alldat$task == 'washrot'), ]
  deleted <- sum(subdat$deleted_trials)
  total <- sum(subdat$total_trials)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
  
  cat('Mirror:\n')
  subdat <- alldat[which(alldat$task == 'mir'), ]
  deleted <- sum(subdat$deleted_trials)
  total <- sum(subdat$total_trials)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
  
  cat('Washout_Mirror:\n')
  subdat <- alldat[which(alldat$task == 'washmir'), ]
  deleted <- sum(subdat$deleted_trials)
  total <- sum(subdat$total_trials)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
  
  cat('TOTAL:\n')
  deleted <- sum(alldat$deleted_trials)
  total <- sum(alldat$total_trials)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
  
  
  cat('TOTAL_Cursor:\n')
  subdat <- alldat[which(alldat$task == 'aligned' | alldat$task == 'rot' | alldat$task == 'mir'), ]
  deleted <- sum(subdat$deleted_trials)
  total <- sum(subdat$total_trials)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
  
  cat('TOTAL_Washout:\n')
  subdat <- alldat[which(alldat$task == 'washrot' | alldat$task == 'washmir'), ]
  deleted <- sum(subdat$deleted_trials)
  total <- sum(subdat$total_trials)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
}