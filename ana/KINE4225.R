source('ana/shared.R')
source('ana/learningRates.R')

#Additional Plots for Neuromatch 2.0 (2020)----
plotPTypeLearningCurvesWONear <- function(perturb = c('ROT', 'MIR'), group = 'noninstructed', target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pres/Fig02_NI_learningcurveWONear.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,61), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 20, 40, 60)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(ptype in perturb){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_%s_CI_learningcurve_WONear.csv', ptype, group))
    
    colourscheme <- getPtypeColourScheme(ptype)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[ptype]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:60), rev(c(1:60))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[ptype]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (ptype in perturb) {
    # plot mean reaches for each group
    col <- colourscheme[[ptype]][['S']]
    lines(meanGroupReaches[[ptype]],col=col,lty=1)
  }
  
  #add legend
  legend(40,-150,legend=c('Rotation','Mirror Reversal'),
         col=c(colourscheme[['ROT']][['S']],colourscheme[['MIR']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Plots for Presentation-----

plotPTypeLearningCurves <- function(perturb = c('ROT', 'MIR'), group = 'noninstructed', target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pres/Fig02_NI_learningcurve.svg', width=12, height=7, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(ptype in perturb){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_%s_CI_learningcurve.csv', ptype, group))
    
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
  legend(65,-150,legend=c('Visuomotor rotation','Mirror reversal'),
         col=c(colourscheme[['ROT']][['S']],colourscheme[['MIR']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotLC <- function(group = 'noninstructed', instmax = 15, location = 'maxvel', targetno = 6, target = 'inline'){
  
  #instmax and noninstmax will differ depending on maximum pp id number in data
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pres/Fig01_NI_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(1,2))
  
  
  plotBlockedIndLC(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'ROT')
  plotBlockedIndLC(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'MIR')
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotPTypeAftereffects <- function(perturb = c('ROT', 'MIR'), group = 'noninstructed', target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pres/Fig03_NI_aftereffects.svg', width=12, height=7, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(ptype in perturb){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_%s_CI_aftereffects.csv', ptype, group))
    
    colourscheme <- getPtypeColourScheme(ptype)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[ptype]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[ptype]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (ptype in perturb) {
    # plot mean reaches for each group
    col <- colourscheme[[ptype]][['S']]
    lines(meanGroupReaches[[ptype]],col=col,lty=1)
  }
  
  #add legend
  legend(35,-150,legend=c('Visuomotor rotation','Mirror reversal'),
         col=c(colourscheme[['ROT']][['S']],colourscheme[['MIR']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotAE <- function(group = 'noninstructed', instmax = 15, location = 'maxvel', targetno = 6, target = 'inline'){
  
  #instmax and noninstmax will differ depending on maximum pp id number in data
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pres/Fig04_NI_BlockedIndAftereffects.svg', width=9, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(1,2))
  
  
  plotBlockedIndRAE(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'ROT')
  plotBlockedIndRAE(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'MIR')
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotRT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/pres/Fig05_NI_reactiontime.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
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

# KINE 4225 Analysis----

getALROTParticipantLearningCurve <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  
  #biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  AT<- getReachAngles(alignedTraining, starttrial=0, endtrial=47, location = location) #rotated is 90 trials; appended to end of aligned
  
  # for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
  #   
  #   target<- biases[biasno, 'targetangle'] #get corresponding target angle
  #   bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
  #   
  #   #subtract bias from reach deviation for rotated session only
  #   RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  # }
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #note that rotation direction is counterbalanced (CCW and CW)
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(AT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      AT$reachdev[which(AT$targetangle == target)] <- ((AT$reachdev[which(AT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      AT$reachdev[which(AT$targetangle == target)] <- (((AT$reachdev[which(AT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  return(AT)
}

getALROTGroupLearningCurves <- function(group, maxppid = 15, location) {
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
    ppangles <- getALROTParticipantLearningCurve(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    rot <- rep(0, 48)
    dat <- cbind(trial, rot, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getAlignedTrialByTrial <- function(group = 'noninstructed', maxppid = 15, location = 'maxvel'){
  
  dat <- getALROTGroupLearningCurves(group=group, maxppid=maxppid, location=location)
  
  for (trialno in dat$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    
    dat[trialno, 3:ncol(dat)] <- ndat
    dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/aligned_trial.csv', row.names = F)
}

getAlignedBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/aligned_trial.csv')
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(0,nrow(ndat))
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/aligned_block.csv', row.names = F)
}

#now do it for ROTATED

getKINEROTParticipantLearningCurve <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 11, task = 'rotation')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 5, task = 'rotation')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=89, location = location) #rotated is 90 trials; appended to end of aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  }
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #note that rotation direction is counterbalanced (CCW and CW)
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(RT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  #RT$reachdev <- ((RT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(RT)
}

getKINEROTGroupLearningCurves <- function(group, maxppid, location) {
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
    ppangles <- getKINEROTParticipantLearningCurve(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    rot <- rep(30, 90)
    dat <- cbind(trial, rot, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

#include RAE data in as well
getKINEROTParticipantAftereffects <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    washoutTrials <- getParticipantTaskData(group, id, taskno = 13, task = 'washout1')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    washoutTrials <- getParticipantTaskData(group, id, taskno = 7, task = 'washout0')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 48 trials
  WT<- getReachAngles(washoutTrials, starttrial=0, endtrial=47, location = location) #washout is same length as aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    WT$reachdev[which(WT$targetangle == target)] <- WT$reachdev[which(WT$targetangle == target)] - bias
  }
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #reachdeviations are all negative values, we just multiply by -1 to make percentage positive
  #we will still get some negative values because of some that go below 0%, but the direction of means if plotted will make more sense
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(WT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      WT$reachdev[which(WT$targetangle == target)] <- ((WT$reachdev[which(WT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  #WT$reachdev <- ((WT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(WT)
}

getKINEROTGroupAftereffects <- function(group, maxppid, location) {
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
    ppangles <- getKINEROTParticipantAftereffects(group=group, id=participant, location = location) #for every participant, get aftereffects data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    rot <- rep(0,48)
    dat <- cbind(trial, rot, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getRotatedTrialByTrial <- function(group = 'noninstructed', maxppid = 15, location = 'maxvel'){
  
  dat <- getKINEROTGroupLearningCurves(group=group, maxppid=maxppid, location=location)
  dat <- as.data.frame(dat)
  
  for (trialno in dat$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    
    dat[trialno, 3:ncol(dat)] <- ndat
    dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  #return(dat)
  
  #include aftereffects (append them by rows)
  dat2 <- getKINEROTGroupAftereffects(group=group, maxppid=maxppid, location=location)
  dat2 <- as.data.frame(dat2)
  
  for (trialno in dat2$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat2 <- as.numeric(dat2[trialno, 3:ncol(dat2)])
    trialmu <- mean(ndat2, na.rm = TRUE)
    trialsigma <- sd(ndat2, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat2[which(abs(ndat2) > trialclip)] <- NA
    
    dat2[trialno, 3:ncol(dat2)] <- ndat2
    dat2$average[trialno] <- mean(as.numeric(dat2[trialno, 3:18]), na.rm = TRUE)
    dat2$sd[trialno] <- sd(as.numeric(dat2[trialno, 3:18]), na.rm = TRUE)
  }
  alldat <- rbind(dat,dat2)
  colnames(alldat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                        "p004", "p005", "p006", "p007", "p008", "p009",
                        "p010", "p011", "p012", "p013", "p014",
                        "p015", "average", "sd")
  write.csv(alldat, file='data/K4225/rotated_trial.csv', row.names = F)
}

getRotatedBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/rotated_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rotA <- rep(30,15)
  rotB <- rep(0,8)
  rot <- c(rotA, rotB)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/rotated_block.csv', row.names = F)
}

#do it for MIRROR
#note that we use 30 degrees here, in order to simplify it and take the average of the three possible target locations: 7.5, 15, 22.5

getKINEMIRParticipantLearningCurve <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 5, task = 'mirror')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(group, id, taskno = 11, task = 'mirror')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=89, location = location) #rotated is 90 trials; appended to end of aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  }
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #note that rotation direction is counterbalanced (CCW and CW)
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(RT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  #RT$reachdev <- ((RT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(RT)
}

getKINEMIRGroupLearningCurves <- function(group, maxppid, location) {
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
    ppangles <- getKINEMIRParticipantLearningCurve(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    rot <- rep(30, 90)
    dat <- cbind(trial, rot, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getKINEMIRParticipantAftereffects <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    washoutTrials <- getParticipantTaskData(group, id, taskno = 7, task = 'washout0')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    washoutTrials <- getParticipantTaskData(group, id, taskno = 13, task = 'washout1')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 48 trials
  WT<- getReachAngles(washoutTrials, starttrial=0, endtrial=47, location = location) #washout is same length as aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    WT$reachdev[which(WT$targetangle == target)] <- WT$reachdev[which(WT$targetangle == target)] - bias
  }
  
  #then for this study we want a measure of percentage of compensation, not angular hand deviation
  #perturbation is constant here (always 30deg), so the (reachdev/30)*100
  #reachdeviations are all negative values, we just multiply by -1 to make percentage positive
  #we will still get some negative values because of some that go below 0%, but the direction of means if plotted will make more sense
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(WT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      WT$reachdev[which(WT$targetangle == target)] <- ((WT$reachdev[which(WT$targetangle == target)])/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  #WT$reachdev <- ((WT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(WT)
}

getKINEMIRGroupAftereffects <- function(group, maxppid, location) {
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
    ppangles <- getKINEMIRParticipantAftereffects(group=group, id=participant, location = location) #for every participant, get aftereffects data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    rot <- rep(0,48)
    dat <- cbind(trial, rot, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getMirroredTrialByTrial <- function(group = 'noninstructed', maxppid = 15, location = 'maxvel'){
  
  dat <- getKINEMIRGroupLearningCurves(group=group, maxppid=maxppid, location=location)
  dat <- as.data.frame(dat)
  
  for (trialno in dat$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    
    dat[trialno, 3:ncol(dat)] <- ndat
    dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  #return(dat)
  
  #include aftereffects (append them by rows)
  dat2 <- getKINEMIRGroupAftereffects(group=group, maxppid=maxppid, location=location)
  dat2 <- as.data.frame(dat2)
  
  for (trialno in dat2$trial){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat2 <- as.numeric(dat2[trialno, 3:ncol(dat2)])
    trialmu <- mean(ndat2, na.rm = TRUE)
    trialsigma <- sd(ndat2, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat2[which(abs(ndat2) > trialclip)] <- NA
    
    dat2[trialno, 3:ncol(dat2)] <- ndat2
    dat2$average[trialno] <- mean(as.numeric(dat2[trialno, 3:18]), na.rm = TRUE)
    dat2$sd[trialno] <- sd(as.numeric(dat2[trialno, 3:18]), na.rm = TRUE)
  }
  alldat <- rbind(dat,dat2)
  colnames(alldat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                        "p004", "p005", "p006", "p007", "p008", "p009",
                        "p010", "p011", "p012", "p013", "p014",
                        "p015", "average", "sd")
  write.csv(alldat, file='data/K4225/mirrored_trial.csv', row.names = F)
}

getMirroredBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/mirrored_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rotA <- rep(30,15)
  rotB <- rep(0,8)
  rot <- c(rotA, rotB)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/mirrored_block.csv', row.names = F)
}

#specify RT for each individual, trial by trial
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
    
    reaction <- ppRT$reaction_time#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    rot <- rep(0, length(reaction))
    dat <- cbind(trial, rot, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getRTAlignedTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getAlignedGroupRTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/RT_aligned_trial.csv', row.names = F)
}

#blocked data can be based off of trial by trial data created

getRTAlignedBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/RT_aligned_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(0,8)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/RT_aligned_block.csv', row.names = F)
}

# Rotated RT
getRotatedGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
    
    reaction <- ppRT$reaction_time#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    rot <- rep(30, length(reaction))
    dat <- cbind(trial, rot, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getRTRotatedTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getRotatedGroupRTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/RT_rotated_trial.csv', row.names = F)
}

getRTRotatedBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/RT_rotated_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(30,15)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/RT_rotated_block.csv', row.names = F)
}

# Mirrored RT
getMirroredGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
    
    reaction <- ppRT$reaction_time#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    rot <- rep(30, length(reaction))
    dat <- cbind(trial, rot, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getRTMirroredTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getMirroredGroupRTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/RT_mirrored_trial.csv', row.names = F)
}

getRTMirroredBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/RT_mirrored_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(30,15)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/RT_mirrored_block.csv', row.names = F)
}

#Washout Rotated RT
getRotatedWashGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
    
    reaction <- ppRT$reaction_time#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    rot <- rep(0, length(reaction))
    dat <- cbind(trial, rot, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getRTRotatedWashTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getRotatedWashGroupRTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/RT_rotwash_trial.csv', row.names = F)
}

getRTRotatedWashBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/RT_rotwash_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(0,8)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/RT_rotwash_block.csv', row.names = F)
}

#Washout Mirror RT
getMirroredWashGroupRTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
    
    reaction <- ppRT$reaction_time#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    rot <- rep(0, length(reaction))
    dat <- cbind(trial, rot, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getRTMirroredWashTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getMirroredWashGroupRTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/RT_mirwash_trial.csv', row.names = F)
}

getRTMirroredWashBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/RT_mirwash_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(0,8)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/RT_mirwash_block.csv', row.names = F)
}

getMTTrials <- function(group, id, task, taskno){
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
    subndat <- subndat[subndat$step == 4,]
    
    if (nrow(subndat)==0){
      movementtime <- NA #will assign NaN if step 4 does not occur
      trial <- trialno
      
    } else{
      firststep4 <- subndat[1,]
      laststep4 <- subndat[nrow(subndat),]
      
      step4start <- firststep4$time_ms
      step4end <- laststep4$time_ms
      
      movementtime <- step4end - step4start
      trial <- trialno
      
    }
    feedback <- c(trial, movementtime, task)
    
    
    if (prod(dim(proportion)) == 0){
      proportion <- feedback
    } else {
      proportion <- rbind(proportion, feedback)
    }
  }
  proportion <- data.frame(proportion, row.names = NULL, stringsAsFactors = F)
  colnames(proportion) <- c('trial', 'movement_time', 'task')
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
    ppMT <- getMTTrials(group = group, id=participant, task = 'aligned', taskno = 1) #for every participant, get MT data
    
    movement <- ppMT$movement_time#get MT column from MT data
    trial <- c(1:length(movement)) #sets up trial column
    rot <- rep(0, length(movement))
    dat <- cbind(trial, rot, movement)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, movement)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getMTAlignedTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getAlignedGroupMTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  # 
  # subdat <- as.matrix(dat[,3:ncol(dat)])
  # subdat[which(subdat > 450)] <- NA
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    #remove all times above 450 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 450)] <- NA
    
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/MT_aligned_trial.csv', row.names = F)
}

#blocked data can be based off of trial by trial data created

getMTAlignedBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/MT_aligned_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(0,8)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/MT_aligned_block.csv', row.names = F)
}

# Rotated RT
getRotatedGroupMTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
      ppMT <- getMTTrials(group=group, id=participant, taskno = 11, task = 'rotation')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppMT <- getMTTrials(group=group, id=participant, taskno = 5, task = 'rotation')
    }
    
    movement <- ppMT$movement_time#get RT column from RT data
    trial <- c(1:length(movement)) #sets up trial column
    rot <- rep(30, length(movement))
    dat <- cbind(trial, rot, movement)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, movement)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getMTRotatedTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getRotatedGroupMTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    #remove all times above 450 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 450)] <- NA
    
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/MT_rotated_trial.csv', row.names = F)
}

getMTRotatedBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/MT_rotated_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(30,15)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/MT_rotated_block.csv', row.names = F)
}

# Mirrored RT
getMirroredGroupMTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
      ppMT <- getMTTrials(group=group, id=participant, taskno = 5, task = 'mirror')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppMT <- getMTTrials(group=group, id=participant, taskno = 11, task = 'mirror')
    }
    
    movement <- ppMT$movement_time#get RT column from RT data
    trial <- c(1:length(movement)) #sets up trial column
    rot <- rep(30, length(movement))
    dat <- cbind(trial, rot, movement)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, movement)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getMTMirroredTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getMirroredGroupMTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    #remove all times above 450 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 2000)] <- NA
    
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/MT_mirrored_trial.csv', row.names = F)
}

getMTMirroredBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/MT_mirrored_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(30,15)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/MT_mirrored_block.csv', row.names = F)
}

#Washout Rotated RT
getRotatedWashGroupMTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
      ppMT <- getMTTrials(group=group, id=participant, taskno = 13, task = 'washout1')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppMT <- getMTTrials(group=group, id=participant, taskno = 7, task = 'washout0')
    }
    
    movement <- ppMT$movement_time#get RT column from RT data
    trial <- c(1:length(movement)) #sets up trial column
    rot <- rep(0, length(movement))
    dat <- cbind(trial, rot, movement)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, movement)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getMTRotatedWashTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getRotatedWashGroupMTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    #remove all times above 450 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 450)] <- NA
    
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/MT_rotwash_trial.csv', row.names = F)
}

getMTRotatedWashBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/MT_rotwash_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(0,8)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/MT_rotwash_block.csv', row.names = F)
}

#Washout Mirror RT
getMirroredWashGroupMTTrials <- function(group = 'noninstructed', maxppid = 15) {
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
      ppMT <- getMTTrials(group=group, id=participant, taskno = 7, task = 'washout0')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppMT <- getMTTrials(group=group, id=participant, taskno = 13, task = 'washout1')
    }
    
    movement <- ppMT$movement_time#get RT column from RT data
    trial <- c(1:length(movement)) #sets up trial column
    rot <- rep(0, length(movement))
    dat <- cbind(trial, rot, movement)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, movement)
    }
    
  }
  #rot <- rep(0,nrow(dataoutput))
  dataoutput <- as.data.frame(dataoutput, stringsAsFactors = FALSE)
  # dataoutput$rot <- rep(0, nrow(dataoutput))
  # dataoutput <- dataoutput[, c("a", "b", "d", "c")]
  return(dataoutput)
  
}

getMTMirroredWashTrialByTrial <- function(group = 'noninstructed', maxppid = 15){
  
  dat <- getMirroredWashGroupMTTrials(group=group, maxppid=maxppid)
  trials <- dat$trial
  
  output <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 3:ncol(dat)])
    #remove all times above 450 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 450)] <- NA
    
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    average <- mean(ndat, na.rm = TRUE)
    sd <- sd(ndat, na.rm = TRUE)
    metrics <- cbind(average, sd)
    
    if (prod(dim(output)) == 0){
      output <- metrics
    } else {
      output <- rbind(output, metrics)
    }
    
    
    dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  
  dat <- cbind(dat, output)
  
  #return(dat)
  colnames(dat) <- c("trial", "rot", "p000", "p001", "p002", "p003",
                     "p004", "p005", "p006", "p007", "p008", "p009",
                     "p010", "p011", "p012", "p013", "p014",
                     "p015", "average", "sd")
  write.csv(dat, file='data/K4225/MT_mirwash_trial.csv', row.names = F)
}

getMTMirroredWashBlocked <- function(){
  
  data <- read.csv(file = 'data/K4225/MT_mirwash_trial.csv')
  
  lastval <- ncol(data) - 2
  subdat <- data[,3:lastval]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  
  
  for (blockno in 1:nrow(ndat)){
    ndat$average[blockno] <- mean(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
    ndat$sd[blockno] <- sd(as.numeric(ndat[blockno,1:16]), na.rm = TRUE)
  }
  
  block <- c(1:nrow(ndat))
  rot <- rep(0,8)
  ndat <- cbind(block, rot, ndat)
  
  write.csv(ndat, file='data/K4225/MT_mirwash_block.csv', row.names = F)
}