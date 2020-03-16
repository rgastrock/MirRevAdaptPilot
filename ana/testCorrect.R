library(scales)
library(svglite)

getCorrectTrials <- function(id, task, taskno){
  #allows for this function to work with each file
  #specify pp id, the task type, and task number
  #note that task type and taskno have to match, depending on present csv files
  #Note to change filepath once data folder is arranged
  dat <- read.csv(file = sprintf('data/pilot/RAW/p00%d/p00%d-%d-%s.csv', id, id, taskno,task))
  #only steps 6 and 7 will have 0 or 1 in trial_column
  ndat <- dat[dat$step == 6 | dat$step == 7, ]
  
  trials <- unique(ndat$trial)
  proportion <- data.frame()
  
  for (trialno in trials){
    
    subndat <- ndat[ndat$trial == trialno,]
    trial <- trialno
    correct <- unique(subndat$trial_correct)
    feedback <- c(trial, correct, task)
    
    if (prod(dim(proportion)) == 0){
      proportion <- feedback
    } else {
      proportion <- rbind(proportion, feedback)
    }
  }
  proportion <- as.data.frame(proportion)
  colnames(proportion) <- c('trial', 'trial_correct', 'task')
  rownames(proportion) <- c()
  
  return(proportion)
}

getProportionCorrect <- function(id, task, taskno){
  
  dat <- getCorrectTrials(id=id, task=task, taskno=taskno)
  propcorrect <- length(which(dat$trial_correct == 1))
  totaltrials <- length(dat$trial)
  percentage <- (propcorrect/totaltrials)*100
  ndat <- data.frame(sprintf('p00%d',id),task,percentage)
  colnames(ndat) <- c('participant', 'task', 'percent_correct')
  
  #do for all tasks and all participants-concatenate to one big file?
  return(ndat)
  
}

getParticipantPropCor <- function(id){
  #if pp id is odd
  #mirror is first than rotation
  if (id%%2 == 1){
    
    out1 <- getProportionCorrect(id = id, task = 'aligned', taskno = 1)
    out2 <- getProportionCorrect(id = id, task = 'random0', taskno = 3)
    out3 <- getProportionCorrect(id = id, task = 'mirror', taskno = 5)
    out4 <- getProportionCorrect(id = id, task = 'washout0', taskno = 7)
    out5 <- getProportionCorrect(id = id, task = 'random1', taskno = 9)
    out6 <- getProportionCorrect(id = id, task = 'rotation', taskno = 11)
    out7 <- getProportionCorrect(id = id, task = 'washout1', taskno = 13)
    
    allout <- rbind(out1,out2,out3,out4,out5,out6,out7)
    
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    out1 <- getProportionCorrect(id = id, task = 'aligned', taskno = 1)
    out2 <- getProportionCorrect(id = id, task = 'random0', taskno = 3)
    out3 <- getProportionCorrect(id = id, task = 'rotation', taskno = 5)
    out4 <- getProportionCorrect(id = id, task = 'washout0', taskno = 7)
    out5 <- getProportionCorrect(id = id, task = 'random1', taskno = 9)
    out6 <- getProportionCorrect(id = id, task = 'mirror', taskno = 11)
    out7 <- getProportionCorrect(id = id, task = 'washout1', taskno = 13)
    
    allout <- rbind(out1,out2,out3,out4,out5,out6,out7)
  }
  return(allout)
}

GetAllPropCor <- function(maxppno){
  
  ppno <- seq(0,maxppno,1)
  alldat <- data.frame()
  for (pp in ppno){
    dat <- getParticipantPropCor(id = pp)
    
    if (prod(dim(alldat)) == 0){
      alldat <- dat
    } else {
      alldat <- rbind(alldat, dat)
    }
  }
  return(alldat)
  #write.csv(alldat, file='data/ProportionCorrectTrials.csv', row.names = F)
  #then can plot this as plot(var$percent_correct ~ var$participant + var$task)
}

# #try to plot it with time that they reach boundary on y axis, two IVs will still be ppid and task----
# getMTTrials <- function(id, task, taskno){
#   #allows for this function to work with each file
#   #specify pp id, the task type, and task number
#   #note that task type and taskno have to match, depending on present csv files
#   #Note to change filepath once data folder is arranged
#   if (id < 10){
#     dat <- read.csv(file = sprintf('data/pilot/RAW/p00%d/p00%d-%d-%s.csv', id, id, taskno,task))
#   } else{
#     dat <- read.csv(file = sprintf('data/pilot/RAW/p0%d/p0%d-%d-%s.csv', id, id, taskno,task))
#   }
#   
#   #only steps 6 and 7 will have 0 or 1 in trial_column
#   ndat <- dat[dat$step == 2 | dat$step == 3 | dat$step == 4, ]
#   
#   trials <- unique(ndat$trial)
#   proportion <- data.frame()
#   
#   for (trialno in trials){
#     
#     subndat <- ndat[ndat$trial == trialno,]
#     subndatstep2 <- subndat[subndat$step == 2,]
#     subndatstep4 <- subndat[subndat$step == 4,]
#     
#     laststep2 <- subndatstep2[nrow(subndatstep2),]
#     step2end <- laststep2$time_ms
#     laststep4 <- subndatstep4[nrow(subndatstep4),]
#     step4end <- laststep4$time_ms
#     
#     movetime <- step4end - step2end
#     trial <- trialno
#     
#     feedback <- c(trial, movetime, task)
#     
#     if (prod(dim(proportion)) == 0){
#       proportion <- feedback
#     } else {
#       proportion <- rbind(proportion, feedback)
#     }
#   }
#   proportion <- data.frame(proportion, row.names = NULL, stringsAsFactors = F)
#   colnames(proportion) <- c('trial', 'movement_time', 'task')
#   #rownames(proportion) <- c()
#   #proportion <- as.data.frame(proportion)
#   return(proportion)
# }
# 
# getMTTasks <- function(id){
#   
#   #if pp id is odd
#   #mirror is first than rotation
#   if (id%%2 == 1){
#     
#     out1 <- getMTTrials(id = id, task = 'aligned', taskno = 1)
#     out2 <- getMTTrials(id = id, task = 'random0', taskno = 3)
#     out3 <- getMTTrials(id = id, task = 'mirror', taskno = 5)
#     out4 <- getMTTrials(id = id, task = 'washout0', taskno = 7)
#     out5 <- getMTTrials(id = id, task = 'random1', taskno = 9)
#     out6 <- getMTTrials(id = id, task = 'rotation', taskno = 11)
#     out7 <- getMTTrials(id = id, task = 'washout1', taskno = 13)
#     
#     allout <- rbind(out1,out2,out3,out4,out5,out6,out7)
#     
#   } else if (id%%2 == 0){
#     #if pp id is even
#     #rotation first then mirror
#     out1 <- getMTTrials(id = id, task = 'aligned', taskno = 1)
#     out2 <- getMTTrials(id = id, task = 'random0', taskno = 3)
#     out3 <- getMTTrials(id = id, task = 'rotation', taskno = 5)
#     out4 <- getMTTrials(id = id, task = 'washout0', taskno = 7)
#     out5 <- getMTTrials(id = id, task = 'random1', taskno = 9)
#     out6 <- getMTTrials(id = id, task = 'mirror', taskno = 11)
#     out7 <- getMTTrials(id = id, task = 'washout1', taskno = 13)
#     
#     allout <- rbind(out1,out2,out3,out4,out5,out6,out7)
#   }
#   return(allout)
# }
# 
# # plotParticipantMovementTime <- function(id){
# #   
# #   df <- getMTTasks(id=id)
# #   #full_trial <- seq(1,length(df$movement_time),1)
# #   #df <- cbind(df,full_trial)
# #   #yupperlim <- max(as.numeric(df$movement_time))
# #   aligneddf <- df[df$task == 'aligned',]
# #   random0df <- df[df$task == 'random0',]
# #   mirrordf <- df[df$task == 'mirror',]
# #   washout0df <- df[df$task == 'washout0',]
# #   random1df <- df[df$task == 'random1',]
# #   rotationdf <- df[df$task == 'rotation',]
# #   washout1df <- df[df$task == 'washout1',]
# #   
# #   par(mfrow=c(4,2), mar=c(2,2,2,2))
# #   plot(aligneddf$movement_time~aligneddf$trial, main='Aligned', xlab = '', ylab ='', ylim = c(200, 1000))
# #   abline(h = c(0,300), col = 8, lty = 2)
# #   abline(h = c(0,700), col = 8, lty = 2)
# #   
# #   plot(random0df$movement_time~random0df$trial, main='Random0', xlab = '', ylab ='', ylim = c(200, 1000))
# #   abline(h = c(0,300), col = 8, lty = 2)
# #   abline(h = c(0,700), col = 8, lty = 2)
# #   
# #   plot(mirrordf$movement_time~mirrordf$trial, main='Mirror', xlab = '', ylab ='', ylim = c(200, 1000))
# #   abline(h = c(0,300), col = 8, lty = 2)
# #   abline(h = c(0,700), col = 8, lty = 2)
# #   
# #   plot(washout0df$movement_time~washout0df$trial, main='Washout0', xlab = '', ylab ='', ylim = c(200, 1000))
# #   abline(h = c(0,300), col = 8, lty = 2)
# #   abline(h = c(0,700), col = 8, lty = 2)
# #   
# #   plot(random1df$movement_time~random1df$trial, main='Random1', xlab = '', ylab ='', ylim = c(200, 1000))
# #   abline(h = c(0,300), col = 8, lty = 2)
# #   abline(h = c(0,700), col = 8, lty = 2)
# #   
# #   plot(rotationdf$movement_time~rotationdf$trial, main='Rotation', xlab = '', ylab ='', ylim = c(200, 1000))
# #   abline(h = c(0,300), col = 8, lty = 2)
# #   abline(h = c(0,700), col = 8, lty = 2)
# #   
# #   plot(washout1df$movement_time~washout1df$trial, main='Washout1', xlab = '', ylab ='', ylim = c(200, 1000))
# #   abline(h = c(0,300), col = 8, lty = 2)
# #   abline(h = c(0,700), col = 8, lty = 2)
# #   
# #   #axis(1, at=c(20,40,60,80,100)) #tick marks for x axis
# #   #axis(2, at = c(200, 300, 700, 1000))
# # }
# 
# plotParticipantMT <- function(id){
#   
#   df <- getMTTasks(id=id)
#   full_trial <- seq(1,length(df$movement_time),1)
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
#   Y <- as.numeric(df$movement_time)
#   
#   plot(c(1:length(df$full_trial)), Y, type = 'n', axes = FALSE,
#        xlab = '', ylab = '', main = sprintf('pp %d: Movement Time (ms) across Trials & Tasks', id),
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
# plotAllMT <- function(maxppno, target='inline'){
#   
#   if (target=='svg') {
#     svglite(file='doc/fig/Fig1_MT.svg', width=18, height=10, pointsize=14, system_fonts=list(sans="Arial"))
#   }
#   
#   ppno <- seq(0,maxppno,1)
#   par(mfrow=c(6,2),mar=c(3,3,3,3))
#   
#   for (pp in ppno){
#     plotParticipantMT(id=pp)
#   }
#   #mtext('Movement Time (ms)', side=2, outer=TRUE, line=-1, cex=1)
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
# }