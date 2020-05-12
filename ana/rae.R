source('ana/shared.R')
source('ana/learningRates.R')

#Aligned data----
getAlignedParticipant <- function(group, id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(group, id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  AT<- getReachAngles(alignedTraining, starttrial = 0, endtrial = 47) #aligned is first 48 trials
  #WT<- getReachAngles(washoutTrials, starttrial=0, endtrial=47, location = location) #washout is same length as aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    AT$reachdev[which(AT$targetangle == target)] <- AT$reachdev[which(AT$targetangle == target)] - bias
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
  
  #WT$reachdev <- ((WT$reachdev * -1)/30)*100
  
  #use below for absolute errors:
  #so we subtract rotation size (30deg) from all reach deviations
  #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
  #RT$reachdev <- RT$reachdev - 30 #if we want positive values
  return(AT)
}

getAlignedGroup <- function(group, maxppid, location) {
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
    
    ppangles <- getAlignedParticipant(group=group, id=participant, location = location) #for every participant, get aftereffects data
    
    
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    dat <- cbind(trial, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getAlignedGroupConfidenceInterval <- function(group, maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getAlignedGroup(group = group, maxppid = maxppid, location = location)
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
      write.csv(confidence, file='data/Aligned_noninstructed_CI.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/Aligned_instructed_CI.csv', row.names = F)
    } 
    
  }
  #}
}

#Reach Aftereffects: ROT -----

#this should be the same as Learning Curves, since all aftereffects measures are in one washout block
#getting mean RAE does not make sense since the deadapted trials will bias the mean
#thus the RAE from early trials of this washout block will not be represented well
#hence, we go for the rate of deadaptation
#essentially same as Learning Curves, just going the opposite direction

getROTParticipantAftereffects <- function(group, id, location) {
  
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

getROTGroupAftereffects <- function(group, maxppid, location) {
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

    ppangles <- getROTParticipantAftereffects(group=group, id=participant, location = location) #for every participant, get aftereffects data

    
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    dat <- cbind(trial, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getROTGroupRAEConfidenceInterval <- function(group, maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getROTGroupAftereffects(group = group, maxppid = maxppid, location = location)
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
      write.csv(confidence, file='data/ROT_noninstructed_CI_aftereffects.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROT_instructed_CI_aftereffects.csv', row.names = F)
    } 
    
  }
  #}
}

plotROTAftereffects <- function(groups = c('noninstructed', 'instructed'),target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4ROT_aftereffects.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Rate of Deadaptation: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_%s_CI_aftereffects.csv', group))
    
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
  legend(35,-100,legend=c('Non-Instructed','Instructed'),
         col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Reach Aftereffects: MIR----
getMIRParticipantAftereffects <- function(group, id, location){
  #same as rotation, we look into percentage of compensation, but note that magnitude to compensate differs per target
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
  WT<- getReachAngles(washoutTrials, starttrial=0, endtrial=47, location = location) #washout same as aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    WT$reachdev[which(WT$targetangle == target)] <- WT$reachdev[which(WT$targetangle == target)] - bias
    
  }
  #after baseline correction, we need to assign specific targets to corresponding magnitudes to compensate
  #we have 24 possible targets, but they differ depending on which side of mirror axis they are (before or after mirror)
  #this will affect calculations later on (due to negative values)
  #so we separate them by amount of compensation, and whether they are before or after mirror axis
  alltargets15bef <- c(82.5, 172.5, 262.5, 352.5) #should compensate for 15 degrees
  alltargets15aft <- c(7.5, 97.5, 187.5, 277.5)
  alltargets30bef <- c(75, 165, 255, 345) #30 degrees
  alltargets30aft <- c(15, 105, 195, 285)
  alltargets45bef <- c(67.5, 157.5, 247.5, 337.5) #45 degrees
  alltargets45aft <- c(22.5, 112.5, 202.5, 292.5)
  
  angles <- unique(WT$targetangle)
  WT['compensate'] <- NA
  
  #we want percentage of compensation
  #we multily by -1 so that getting positive values mean that the hand went to the correct direction
  #above 100 values would mean an overcompensation, 0 is going directly to target, negative values are undercompensation
  for (target in angles){
    if (target %in% alltargets15bef){
      WT$reachdev[which(WT$targetangle == target)] <- ((WT$reachdev[which(WT$targetangle == target)])/15)*100
      WT$compensate[which(WT$targetangle == target)] <- 15
    } else if (target %in% alltargets15aft){
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/30)*100
      WT$compensate[which(WT$targetangle == target)] <- 15
    } else if (target %in% alltargets30bef){
      WT$reachdev[which(WT$targetangle == target)] <- ((WT$reachdev[which(WT$targetangle == target)])/30)*100
      WT$compensate[which(WT$targetangle == target)] <- 30
    } else if (target %in% alltargets30aft){
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/30)*100
      WT$compensate[which(WT$targetangle == target)] <- 30
    } else if (target %in% alltargets45bef){
      WT$reachdev[which(WT$targetangle == target)] <- ((WT$reachdev[which(WT$targetangle == target)])/45)*100
      WT$compensate[which(WT$targetangle == target)] <- 45
    } else if (target %in% alltargets45aft){
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/45)*100
      WT$compensate[which(WT$targetangle == target)] <- 45
    }
  }
  #write.csv(RT, file='data/PPLCmir.csv', row.names = F)
  return(WT)  
}

getMIRGroupAftereffects <- function(group, maxppid, location) { # add angle?
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
    ppangles <- getMIRParticipantAftereffects(group=group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    dat <- cbind(trial, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getMIRGroupRAEConfidenceInterval <- function(group, maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  #compangle <- c(15,30,45)
  #for (comp in compangle){
  data <- getMIRGroupAftereffects(group = group, maxppid = maxppid, location = location) #angle = comp
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
      write.csv(confidence, file='data/MIR_noninstructed_CI_aftereffects.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIR_instructed_CI_aftereffects.csv', row.names = F)
    }
  }
  #}
}


plotMIRAftereffects <- function(groups = c('noninstructed', 'instructed'),target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4MIR_aftereffects.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Rate of Deadaptation: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_aftereffects.csv', group))
    
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
  legend(35,-100,legend=c('Non-Instructed','Instructed'),
         col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotAftereffects <- function(target='inline'){
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4_aftereffects.svg', width=7, height=10, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow = c(2,1))
  
  plotROTAftereffects()
  plotMIRAftereffects()
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

# Individual data: Aftereffects----

#first split 48 trials into sets of 6 trials each
#then in each set, plot individual data as lines

getBlockedIndividualAftereffects <- function(group, maxppid, location, targetno, perturb){
  
  if (perturb == 'ROT'){
    data <- getROTGroupAftereffects(group = group, maxppid = maxppid, location = location)
  } else if (perturb == 'MIR'){
    data <- getMIRGroupAftereffects(group = group, maxppid = maxppid, location = location)
  }
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- targetno;
  ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  ndat$trial <- c(1:length(ndat$trial))
  
  #but data is in wide format, we would want it in long format
  #this requires library(tidyr)
  ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
  ndat_long$participant <- as.character(ndat_long$participant)
  
  participants <- unique(ndat_long$participant)
  
  
  PPindex <- 0
  
  for (pp in participants) {
    row.idx <- which(ndat_long$participant == pp)
    ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
    
    PPindex <- PPindex + 1
  }
  
  return(ndat_long)
}

plotBlockedIndRAE <- function(group, maxppid, location, targetno, perturb, target='inline'){
  
  if (perturb == 'ROT'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/Fig5ROT_BlockedIndAftereffects.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualAftereffects(group = group, maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    #remove pp004 because they anti-learned
    #data <- subset(data, participant != 'pp4')
    
    plot(NA, NA, xlim = c(0,9), ylim = c(-200,200), 
         xlab = "Block", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Blocked Individual Aftereffects: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = '#000000', lty = 2)
    axis(1, at=c(1:8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200)) #tick marks for y axis
    
    
    participants <- unique(data$participant)
    #linetypeidx <- 1
    
    #library(RColorBrewer)
    #all palette available from RColorBrewer
    #display.brewer.all()
    #we will select the first n colors in the Set1 palette, depending on how many pp we have
    #cols<-brewer.pal(n=maxppid+1,name="Set1")
    #cols contain the names of n different colors
    #colidx <- 1
    colourscheme <- getPtypeColourScheme(perturb = perturb)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[perturb]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      points(data$trial[row.idx],data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      
      #linetypeidx <- linetypeidx + 1
      #colidx <- colidx +1
    }
    
    #then create a mean for all, according to trial number
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[perturb]][['S']]
      lines(x=rep(block,2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
      #print(meandist$CI95)
      points(x=block,y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
    }
    
    lines(x=c(1:length(blockno)),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    
    
    # blockno <- unique(data$trial)
    # allmeans <- data.frame()
    # for (block in blockno){
    #   row.idx <- which(data$trial == block)
    #   blockmean <- data$reachdev[row.idx]
    #   val <- mean(blockmean, na.rm = TRUE)
    #   
    #   if (prod(dim(allmeans)) == 0){
    #     allmeans <- val
    #   } else {
    #     allmeans <- rbind(allmeans, val)
    #   }
    # }
    # col <- colourscheme[[perturb]][['S']]
    # lines(c(1:length(allmeans)),allmeans[,1], lwd = 2, lty = 1, col = col)
    # points(c(1:length(allmeans)),allmeans[,1], pch = 19, col = col)
    
    #legend(12,-100,legend=c('Implicit 30°','Strategy 30°','Cursor Jump', 'Hand View'),
    #      col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
    #     lty=1,bty='n',cex=1)
  } else if (perturb == 'MIR'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/Fig5MIR_BlockedIndAftereffects.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualAftereffects(group = group, maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    #remove pp004 because they anti-learned
    #data <- subset(data, participant != 'pp4')
    
    plot(NA, NA, xlim = c(0,9), ylim = c(-200,200), 
         xlab = "Block", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Blocked Individual Aftereffects: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = '#000000', lty = 2)
    axis(1, at=c(1:8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200)) #tick marks for y axis
    
    
    participants <- unique(data$participant)
    #linetypeidx <- 1
    
    #library(RColorBrewer)
    #all palette available from RColorBrewer
    #display.brewer.all()
    #we will select the first n colors in the Set1 palette, depending on how many pp we have
    #cols<-brewer.pal(n=maxppid+1,name="Set1")
    #cols contain the names of n different colors
    #colidx <- 1
    colourscheme <- getPtypeColourScheme(perturb = perturb)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[perturb]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      points(data$trial[row.idx],data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      
      #linetypeidx <- linetypeidx + 1
      #colidx <- colidx +1
    }
    
    #then create a mean for all, according to trial number
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[perturb]][['S']]
      lines(x=rep(block,2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
      #print(meandist$CI95)
      points(x=block,y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
    }
    
    lines(x=c(1:length(blockno)),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    
    
    # blockno <- unique(data$trial)
    # allmeans <- data.frame()
    # for (block in blockno){
    #   row.idx <- which(data$trial == block)
    #   blockmean <- data$reachdev[row.idx]
    #   val <- mean(blockmean, na.rm = TRUE)
    #   
    #   if (prod(dim(allmeans)) == 0){
    #     allmeans <- val
    #   } else {
    #     allmeans <- rbind(allmeans, val)
    #   }
    # }
    # col <- colourscheme[[perturb]][['S']]
    # lines(c(1:length(allmeans)),allmeans[,1], lwd = 2, lty = 1, col = col)
    # points(c(1:length(allmeans)),allmeans[,1], pch = 19, col = col)
    
    #legend(12,-100,legend=c('Implicit 30°','Strategy 30°','Cursor Jump', 'Hand View'),
    #      col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
    #     lty=1,bty='n',cex=1)
  }
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotROTMIRRAE <- function(groups = c('noninstructed'), noninstmax = 15, instmax = 31, location = 'maxvel', targetno = 6, target = 'inline'){
  #need to indicate non instructed and instructed in title
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5_BlockedIndAftereffects.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(1,2))
  
  for(group in groups){
    #if(group == 'noninstructed'){
      plotBlockedIndRAE(group=group, maxppid=noninstmax, location =location, targetno = targetno, perturb = 'ROT')
      plotBlockedIndRAE(group=group, maxppid=noninstmax, location =location, targetno = targetno, perturb = 'MIR')
    #} else if (group == 'instructed'){
    #   plotBlockedIndRAE(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'ROT')
    #   plotBlockedIndRAE(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'MIR')
    # }
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Reach AFtereffects: STATS----
#Analysis should be similar to learning curves, with the addition of confirming presence of RAE for at least the first block.
#Stats are currently only for noninstructed group

#First, we need the aligned data as well

getRAELongFormat <- function(groups = c('noninstructed','instructed'), maxppid = 15, location = 'maxvel'){
  
  for (group in groups){
    #Aligned data
    ALdat <- getAlignedGroup(group=group,maxppid=maxppid,location=location)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(ALdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    ALdat <- as.data.frame(ALdat)
    perturb <- rep('AL', nrow(ALdat))
    ALdat <- cbind(ALdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longALdata <- gather(ALdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longALdata, file=sprintf('data/ALIGNED_%s_long.csv', group), row.names = F)
    
    
    #Rotation data
    ROTdat <- getROTGroupAftereffects(group=group,maxppid=maxppid,location=location)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(ROTdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    ROTdat <- as.data.frame(ROTdat)
    perturb <- rep('ROT', nrow(ROTdat))
    ROTdat <- cbind(ROTdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longROTdata <- gather(ROTdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longROTdata, file=sprintf('data/ROT_%s_aftereffects_long.csv', group), row.names = F)
    
    #Mirror data
    MIRdat <- getMIRGroupAftereffects(group=group,maxppid=maxppid,location=location)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(MIRdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    MIRdat <- as.data.frame(MIRdat)
    perturb <- rep('MIR', nrow(MIRdat))
    MIRdat <- cbind(MIRdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longMIRdata <- gather(MIRdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longMIRdata, file=sprintf('data/MIR_%s_aftereffects_long.csv', group), row.names = F)
  }
}

#First, we confirm the presence of aftereffects
getBlockedAlignedData <- function(group = 'noninstructed', blockdefs){
  LCaov <- data.frame()
  curves <- read.csv('data/ALIGNED_noninstructed_long.csv', stringsAsFactors=FALSE)  
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

getBlockedRAEAOV <- function(perturbations = c('ROT','MIR'), group = 'noninstructed', blockdefs) {
  #function reads in aftereffects_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  #to include instructed group, just create another for loop here
  for (perturb in perturbations){  
    curves <- read.csv(sprintf('data/%s_noninstructed_aftereffects_long.csv',perturb), stringsAsFactors=FALSE)  
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

RAEt.test <- function() {
  
  blockdefs <- list('first'=c(1,12),'second'=c(13,12),'last'=c(37,12))
  LC4test1 <- getBlockedAlignedData(blockdefs=blockdefs)
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6)) #6 trials per block
  LC4test2 <- getBlockedRAEAOV(blockdefs=blockdefs)
  
  LC4test <- rbind(LC4test1, LC4test2)
  LC4test$participant <- as.factor(LC4test$participant)
  
  ALdat <- LC4test[which(LC4test$block == 'last' & LC4test$perturbtype == 'AL'),]
  ROTdat <- LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'ROT'),]
  MIRdat <-LC4test[which(LC4test$block == 'first' & LC4test$perturbtype == 'MIR'),]
  
  cat('Aligned (last block) compared to Rotation (first block):\n')
  print(t.test(ALdat$compensation, ROTdat$compensation, paired = TRUE))
  # cat('Effect Size - Cohen d:\n')
  # print(cohensD(subdf$pred_update, mu=0))
  
  cat('Aligned (last block) compared to Mirror (first block):\n')
  print(t.test(ALdat$compensation, MIRdat$compensation, paired = TRUE))
  # cat('Effect Size - Cohen d:\n')
  # print(cohensD(subdf$pred_update, mu=0))
  
}

#Rotation differs from aligned, but Mirror does not. So we can then look into the difference of ROT from MIR
#similar to how we analyzed learning curves.

#Below are stats similar to learning curves. Looking into washout at three different blocks of washout training.
reachaftereffectsANOVA <- function() {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6)) #6 trials per block
  
  LC4aov <- getBlockedRAEAOV(blockdefs=blockdefs)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=c(perturbtype, block),type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}

RAEComparisonMeans <- function(){
  
  #can plot interaction just to eyeball it:
  plot(interactionMeans(lm(compensation ~ block * perturbtype, data=LC4aov), factors=c('perturbtype', 'block'), atx='block'))
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6))
  
  LC4aov <- getBlockedRAEAOV(blockdefs=blockdefs) 
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

RAEComparisonsAllBlocks <- function(method='bonferroni'){
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(43,6))
  
  LC4aov <- getBlockedRAEAOV(blockdefs=blockdefs) 
  secondAOV <- aov_ez("participant","compensation",LC4aov,within=c("perturbtype","block"))
  #based on cellmeans, confidence intervals and plots give us an idea of what contrasts we want to compare
  
  ROT_firstvsROT_second  <- c(1,0,-1,0,0,0)
  ROT_firstvsROT_last    <- c(1,0,0,0,-1,0)
  MIR_firstvsMIR_second  <- c(0,1,0,-1,0,0)
  MIR_firstvsMIR_last    <- c(0,1,0,0,0,-1)
  ROT_firstvsMIR_first   <- c(1,-1,0,0,0,0)
  ROT_secondvsMIR_second <- c(0,0,1,-1,0,0)
  ROT_lastvsMIR_last     <- c(0,0,0,0,1,-1)
  
  contrastList <- list('Block1: ROT vs. Block2: ROT'=ROT_firstvsROT_second, 'Block1: ROT vs. Block3: ROT'=ROT_firstvsROT_last,
                       'Block1: MIR vs. Block2: MIR'=MIR_firstvsMIR_second, 'Block1: MIR vs. Block3: MIR'=MIR_firstvsMIR_last,
                       'Block1: ROT vs. MIR'=ROT_firstvsMIR_first, 'Block2: ROT vs. MIR'=ROT_secondvsMIR_second, 'Block3: ROT vs. MIR'=ROT_lastvsMIR_last)
  
  comparisons<- contrast(emmeans(secondAOV$aov,specs=c('perturbtype','block')), contrastList, adjust=method)
  
  print(comparisons)
}

#ROT differs from MIR for the first 2 blocks, but are not different at the end.
# identifying where they stop being different, can easily be seen in the plots.
#Lack of difference for the three blocks of MIR suggest it did not differ from aligned, but ROT did.



