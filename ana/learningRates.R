source('ana/shared.R')

#Learning Rates----

getTrialReachAngleAt <- function(trialdf, location = 'maxvel') {
  
  
  # location (string) determines where the angle of thereach is determines, it is one of:
  # maxvel: maximum velocity (default)
  # endpoint: end of the reach
  # cmX: the last sample before this distance from home, where X is replaced by a numeral
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=1)
  
  # if the trial was rejected, return empty matrix now
  if (trialdf[1,'trialselected_bool'] == 0) {
    
    return(reachangle);
    
  }
  
  # extract the relevant reach information
  X <- trialdf[trialdf$sampleselected_bool == 1,'mousex_cm']
  Y <- trialdf[trialdf$sampleselected_bool == 1,'mousey_cm']
  MV <- trialdf[trialdf$sampleselected_bool == 1,'maxvelocity_idx']
  angle <- trialdf[1,'targetangle_deg']
  
  # print(X)
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateTrajectory(X,Y,-1*angle)
  X <- trajectory[,1]
  Y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be in the data
  if (location == 'maxvel') {
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  # end point, just the last point in the selected stretch of the reach
  if (location == 'endpoint') {
    rown <- length(X)
    invalidlocation <- FALSE
  }
  # cutoff in centimers, the last sample before this cutoff distance is reached
  # this assumes that people don't go back, or that there is only one movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(X^2 + Y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  # if we don't have a valid location, we can't calculate an angle to return
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  #reachangle[1,2] <- angle #I don't know why I have to remove this for it to work!But it's the only thing keeping this function from being generic
  
  return(reachangle)
  
}

getReachAngles <- function(df, starttrial=0, endtrial=NULL, location = 'maxvel') {
  
  trialnumbers <- c(starttrial:endtrial)
  
  #place holders for variables in data frame
  trial <- c()
  targetangle <- c()
  reachdev <- c()
  
  for (trialnumber in trialnumbers) {
    
    indices <- which(df$trial == trialnumber) #rows of current trial
    
    if (length(indices) > 0) { 
      
      trialdf <- subset(df, trial == trialnumber) #get current trial number
      targetangle <- c(targetangle, trialdf$targetangle_deg[1]%%360) #target angle in degrees (all 12 for aligned); 1 is the index to get this value
      reachdev <- c(reachdev, getTrialReachAngleAt(trialdf, location = location)) #relies on reach deviation function
      trial <- c(trial, trialnumber) #counter to keep going
      
    } else {
      #set values to NA if not greater than zero
      #this part helps to fill in missing values in data
      targetangle <- c(targetangle, NA)
      reachdev <- c(reachdev, NA)
      trial <- c(trial, trialnumber) #trial numbers would still be displayed (i.e., is not NA)
      
    }
    
    
  }
  
  #build a data frame
  angularreachdeviations <- data.frame(trial, targetangle, reachdev)
  return(angularreachdeviations)
  
}

getAlignedTrainingBiases <- function(df, location) {
  
  #trials are 0 to 47
  df <- getReachAngles(df=df, starttrial = 0, endtrial = 47, location = location) 
  #get median reachdev for each angle
  trainingBiases <- aggregate(reachdev ~ targetangle, data= df, FUN = median) 
  return(trainingBiases)
  
}

plotLearningCurves <- function(target='inline'){
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_learningcurve.svg', width=7, height=10, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow = c(2,1))
  
  plotROTLearningCurves()
  plotMIRLearningCurves()
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

# Learning Curves ROTATION----
getROTParticipantLearningCurve <- function(group, id, location) {
  
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

getROTGroupLearningCurves <- function(group, maxppid, location) {
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
    ppangles <- getROTParticipantLearningCurve(group = group, id=participant, location = location) #for every participant, get learning curve data
    
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

getROTGroupConfidenceInterval <- function(group, maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getROTGroupLearningCurves(group = group, maxppid = maxppid, location = location)
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
      write.csv(confidence, file='data/ROT_noninstructed_CI_learningcurve.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROT_instructed_CI_learningcurve.csv', row.names = F)
    }
    
  }
  #}
}

plotROTLearningCurves <- function(groups = c('noninstructed'),target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_ROT_learningcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_%s_CI_learningcurve.csv', group))
    
    colourscheme <- getColourScheme(groups = group)
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
  # legend(70,-100,legend=c('Non-Instructed','Instructed'),
  #        col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


# Learning Curves MIRROR----
getMIRParticipantLearningCurve <- function(group, id, location){
  #same as rotation, we look into percentage of compensation, but note that magnitude to compensate differs per target
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
  
  angles <- unique(RT$targetangle)
  RT['compensate'] <- NA
  
  #we want percentage of compensation
  #we multily by -1 so that getting positive values mean that the hand went to the correct direction
  #above 100 values would mean an overcompensation, 0 is going directly to target, negative values are undercompensation
  for (target in angles){
    if (target %in% alltargets15bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets15aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets30bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets30aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets45bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    } else if (target %in% alltargets45aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    }
  }
  #write.csv(RT, file='data/PPLCmir.csv', row.names = F)
  return(RT)  
}


getMIRGroupLearningCurves <- function(group, maxppid, location) { # add angle?
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
    ppangles <- getMIRParticipantLearningCurve(group = group, id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    dat <- cbind(trial, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
    # if (angle == 15){
    #   ppangles <- subset(ppangles, compensate == 15)
    #   reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    #   trial <- c(1:length(reaches)) #sets up trial column
    #   dat <- cbind(trial, reaches)
    #   #rdat <- dat$reaches
    #   
    #   if (prod(dim(dataoutput)) == 0){
    #     dataoutput <- dat
    #   } else {
    #     dataoutput <- cbind(dataoutput, reaches)
    #   }
    # } else if (angle == 30){
    #   ppangles <- subset(ppangles, compensate == 30)
    #   reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    #   trial <- c(1:length(reaches)) #sets up trial column
    #   dat <- cbind(trial, reaches)
    #   #rdat <- dat$reaches
    #   
    #   if (prod(dim(dataoutput)) == 0){
    #     dataoutput <- dat
    #   } else {
    #     dataoutput <- cbind(dataoutput, reaches)
    #   }
    # } else if (angle == 45){
    #   ppangles <- subset(ppangles, compensate == 45)
    #   reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    #   trial <- c(1:length(reaches)) #sets up trial column
    #   dat <- cbind(trial, reaches)
    #   #rdat <- dat$reaches
    #   
    #   if (prod(dim(dataoutput)) == 0){
    #     dataoutput <- dat
    #   } else {
    #     dataoutput <- cbind(dataoutput, reaches)
    #   }
    # }
    
    
    
  }
  return(dataoutput)
}

getMIRGroupConfidenceInterval <- function(group, maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  #compangle <- c(15,30,45)
  #for (comp in compangle){
  data <- getMIRGroupLearningCurves(group = group, maxppid = maxppid, location = location) #angle = comp
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
      write.csv(confidence, file='data/MIR_noninstructed_CI_learningcurve.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIR_instructed_CI_learningcurve.csv', row.names = F)
    }
  }
  #}
}

plotMIRLearningCurves <- function(groups = c('noninstructed'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_MIR_learningcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_learningcurve.csv', group))
    
    colourscheme <- getColourScheme(groups = group)
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
  # legend(70,-100,legend=c('Non-Instructed','Instructed'),
  #        col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Learning Curves WITHOUT NEAR TARGET----

getROTParticipantLearningCurveWONear <- function(group, id, location) {
  
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
  
  #then we remove all data from targets near the mirror axis. Have to do this for rotated as well, to make the two data sets comparable.
  removetargets <- c(7.5, 82.5, 97.5, 172.5, 187.5, 262.5, 277.5, 352.5)
  RT <- RT[-which(RT$targetangle %in% removetargets),]
  
  
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

getROTGroupLearningCurvesWONear <- function(group, maxppid, location) {
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
    ppangles <- getROTParticipantLearningCurveWONear(group = group, id=participant, location = location) #for every participant, get learning curve data
    
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

getROTGroupConfidenceIntervalWONear <- function(group, maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getROTGroupLearningCurvesWONear(group = group, maxppid = maxppid, location = location)
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
      write.csv(confidence, file='data/ROT_noninstructed_CI_learningcurve_WONear.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/ROT_instructed_CI_learningcurve_WONear.csv', row.names = F)
    }
    
  }
  #}
}

plotROTLearningCurvesWONear <- function(groups = c('noninstructed','instructed'),target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_ROT_learningcurveWONear.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,61), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 20, 40, 60)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_%s_CI_learningcurve_WONear.csv', group))
    
    colourscheme <- getColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:60), rev(c(1:60))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
  }
  
  #add legend
  legend(40,-100,legend=c('Non-Instructed','Instructed'),
         col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRParticipantLearningCurveWONear <- function(group, id, location){
  #same as rotation, we look into percentage of compensation, but note that magnitude to compensate differs per target
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
  
  #then we remove all data from targets near the mirror axis. Have to do this for rotated as well, to make the two data sets comparable.
  removetargets <- c(7.5, 82.5, 97.5, 172.5, 187.5, 262.5, 277.5, 352.5)
  RT <- RT[-which(RT$targetangle %in% removetargets),]
  
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
  
  angles <- unique(RT$targetangle)
  RT['compensate'] <- NA
  
  #we want percentage of compensation
  #we multily by -1 so that getting positive values mean that the hand went to the correct direction
  #above 100 values would mean an overcompensation, 0 is going directly to target, negative values are undercompensation
  for (target in angles){
    if (target %in% alltargets15bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets15aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets30bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets30aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets45bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    } else if (target %in% alltargets45aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    }
  }
  #write.csv(RT, file='data/PPLCmir.csv', row.names = F)
  return(RT)  
}

getMIRGroupLearningCurvesWONear <- function(group, maxppid, location) { # add angle?
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
    ppangles <- getMIRParticipantLearningCurveWONear(group = group, id=participant, location = location) #for every participant, get learning curve data
    
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

getMIRGroupConfidenceIntervalWONear <- function(group, maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  #compangle <- c(15,30,45)
  #for (comp in compangle){
  data <- getMIRGroupLearningCurvesWONear(group = group, maxppid = maxppid, location = location) #angle = comp
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
      write.csv(confidence, file='data/MIR_noninstructed_CI_learningcurve_WONear.csv', row.names = F) 
    } else if (group == 'instructed'){
      write.csv(confidence, file='data/MIR_instructed_CI_learningcurve_WONear.csv', row.names = F)
    }
  }
  #}
}

plotMIRLearningCurvesWONear <- function(groups = c('noninstructed','instructed'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_MIR_learningcurveWONear.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,61), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 20, 40, 60)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_learningcurve_WONear.csv', group))
    
    colourscheme <- getColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:60), rev(c(1:60))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
  }
  
  #add legend
  legend(40,-100,legend=c('Non-Instructed','Instructed'),
         col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Learning Curves Individual Target Analysis for Mirror----
# Assess high variance in Mir reversal, by looking into learning curves for each target (7.5, 15, 22.5)
getMIRParticipantTargetCurve <- function(group, id, location){
  #same as rotation, we look into percentage of compensation, but note that magnitude to compensate differs per target
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
  
  angles <- unique(RT$targetangle)
  RT['compensate'] <- NA
  
  #we want percentage of compensation
  #we multily by -1 so that getting positive values mean that the hand went to the correct direction
  #above 100 values would mean an overcompensation, 0 is going directly to target, negative values are undercompensation
  for (target in angles){
    if (target %in% alltargets15bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets15aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets30bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets30aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets45bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    } else if (target %in% alltargets45aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    }
  }
  #write.csv(RT, file='data/PPLCmir.csv', row.names = F)
  return(RT)  
}

#need a function that would collect all data from all participants into one, while still separating info for each target
getMIRGroupTargetCurve <- function(group, maxppid, location){
  
  if (group == 'noninstructed'){
    participants <- seq(0,maxppid,1)
  } else if (group == 'instructed'){
    participants <- seq(16,maxppid,1)
  }
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppangles <- getMIRParticipantTargetCurve(group = group, id=participant, location = location) #for every participant, get learning curve data
    participant <- rep(participant, nrow(ppangles))
    dat <- cbind(participant, ppangles)
    
    # reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    # trial <- c(1:length(reaches)) #sets up trial column
    # dat <- cbind(trial, reaches)
    # #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- rbind(dataoutput, dat)
    }
  }
  return(dataoutput)
}

getMIRTargetCurve <- function(group, maxppid, location, angle){
  
  #each column will be one participant and their 90 trials
  dat <- getMIRGroupTargetCurve(group=group, maxppid=maxppid,location=location)

  #return whichever data is needed
  #angle is either 15, 30, 45 (or the amount of compensation)
  if (angle == 15){
    subdat <- dat[which(dat$compensate == 15), ]
  } else if (angle == 30){
    subdat <- dat[which(dat$compensate == 30), ]
  } else if (angle == 45){
    subdat <- dat[which(dat$compensate == 45), ]
  }
  
  #get only columns we are interested in then transform to wide format, so that we can generate learning curves using code we already have
  subdat <- subdat[,c(1,4)]
  participants <- unique(subdat$participant)
  
  newdat <- data.frame()
  for(pp in participants){
    reachdev <- subdat[which(subdat$participant == pp),]
    reachdev <- reachdev[,-1]
    trial <- c(1:length(reachdev))
    output <- cbind(trial, reachdev)
    
    if (prod(dim(newdat)) == 0){
      newdat <- output
    } else {
      newdat <- cbind(newdat, reachdev)
    }
  }
  return(newdat)
}

getMIRGroupTargetCurveConfidenceInterval <- function(group, maxppid, location, angles = c(15,30,45), type){
  
  for(angle in angles){
    data <- getMIRTargetCurve(group = group, maxppid = maxppid, location = location, angle=angle) #angle = comp
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
        write.csv(confidence, file=sprintf('data/MIR_noninstructed_CI_targetcurve%s.csv', angle), row.names = F) 
      } else if (group == 'instructed'){
        write.csv(confidence, file=sprintf('data/MIR_instructed_CI_targetcurve%s.csv', angle), row.names = F)
      }
    }
  }
}

#for simplicity, I will make 2 functions that will generate order effects plots for non instructed and instructed groups separately
plotNIMIRTargetCurve<- function(group = 'noninstructed', angles = c(15,30,45), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_MIR_NI_targetcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,31), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Learning Rate Across Targets", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 10, 20, 30)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(angle in angles){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_targetcurve%s.csv', group, angle))
    
    colourscheme <- getTtypeColourScheme(angles = angle)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[angle]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:30), rev(c(1:30))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[angle]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (angle in angles) {
    # plot mean reaches for each group
    col <- colourscheme[[angle]][['S']]
    lines(meanGroupReaches[[angle]],col=col,lty=1)
  }
  
  #add legend
  legend(20,-100,legend=c('15 deg','30 deg','45 deg'),
         col=c(colourscheme[[15]][['S']],colourscheme[[30]][['S']],colourscheme[[45]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIMIRTargetCurve<- function(group = 'instructed', angles = c(15,30,45), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_MIR_I_targetcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,31), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Learning Rate Across Targets", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 10, 20, 30)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(angle in angles){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_%s_CI_targetcurve%s.csv', group, angle))
    
    colourscheme <- getTtypeColourScheme(angles = angle)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[angle]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:30), rev(c(1:30))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[angle]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (angle in angles) {
    # plot mean reaches for each group
    col <- colourscheme[[angle]][['S']]
    lines(meanGroupReaches[[angle]],col=col,lty=1)
  }
  
  #add legend
  legend(20,-100,legend=c('15 deg','30 deg','45 deg'),
         col=c(colourscheme[[15]][['S']],colourscheme[[30]][['S']],colourscheme[[45]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Much of the variability seems to be due to the 15 deg separation (near mirror axis). So we can look into this further.
#generate boxplots for each trial, showing the distribution of compensation across participants for that trial.
#vioplots are also generated. Nothing really bimodal, but there are what seem to be some skewed distributions.
#Switching between maxvel and endpoint does not affect the distributions that much. See three functions below.

getTargetCurveBoxplots <- function(group, maxppid, location, angles = c(15,30,45)){
  
  #par(mfrow = c(3,1))
  
  for(angle in angles){
    dat <- as.data.frame(getMIRTargetCurve(group=group, maxppid = maxppid, location=location, angle=angle))
    #transform to long format so we can do boxplots for every trial
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(dat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    longdata <- gather(dat, participant, reachdev, ppcols[1:length(ppcols)], factor_key=TRUE)
    
    #there are outliers, but to get a better view, can set outline=False to not include these in the plot
    boxplot(reachdev~trial, data=longdata, ylab='Compensation (%)', xlab='Trial', main=sprintf('Target Separation: %s deg.', angle),
            axes=FALSE,outline=FALSE)
    axis(1, at=1:30)
    axis(2, at = c(-1000,-400,-250,-100,0,100,250,400,1000))
  }
}

getTargetCurveVioplots <- function(group, maxppid, location, angles = c(15,30,45)){
  
  #par(mfrow = c(3,1))
  
  for(angle in angles){
    dat <- as.data.frame(getMIRTargetCurve(group=group, maxppid = maxppid, location=location, angle=angle))
    #transform to long format so we can do boxplots for every trial
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(dat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    longdata <- gather(dat, participant, reachdev, ppcols[1:length(ppcols)], factor_key=TRUE)
    
    #there are outliers, but to get a better view, can set outline=False to not include these in the plot
    vioplot(reachdev~trial, data=longdata, col='#005de42f', lty=1, border=NA, drawRect=T, rectCol='black', 
            lineCol='black', axes=F, side='left', xaxt='n',yaxt='n',
            ylab='Compensation (%)', xlab='Trial', main=sprintf('Target Separation: %s deg.', angle)) #pchMed='-')
    axis(1, at=1:30)
    axis(2, at = c(-1000,-400,-250,-100,0,100,250,400,1000))
  }
}

testTargetCurveMeasure <- function(group, maxppid, angles = c(15,30,45)){
  
  for(angle in angles){
    #data based on maxvel
    datmaxvel <- as.data.frame(getMIRTargetCurve(group=group, maxppid = maxppid, location='maxvel', angle=angle))
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(datmaxvel) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    maxvel <- gather(datmaxvel, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    #data based on endpt
    datendpt  <- as.data.frame(getMIRTargetCurve(group=group, maxppid = maxppid, location='endpoint', angle=angle))
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(datendpt) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    endpt <- gather(datendpt, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    #compare
    plot(maxvel$compensation, endpt$compensation, main=sprintf('maxvel and endpt correlation for target separated by %s degrees', angle))
    cat(sprintf('maxvel and endpoint correlation for target separated by %s degrees:\n', angle))
    print(cor.test(maxvel$compensation, endpt$compensation))
  }
}

# Learning Curves INDIVIDUAL DATA----

#first split 90 trials into sets of 6 trials each
#then in each set, plot individual data as lines

getBlockedIndividualLearningCurves <- function(group, maxppid, location, targetno, perturb){
  
  if (perturb == 'ROT'){
    data <- getROTGroupLearningCurves(group = group, maxppid = maxppid, location = location)
  } else if (perturb == 'MIR'){
    data <- getMIRGroupLearningCurves(group = group, maxppid = maxppid, location = location)
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

# plotUncleanedBlockedIndLC <- function(group, maxppid, location, targetno, target='inline'){
#   
#   #but we can save plot as svg file
#   if (target=='svg') {
#     svglite(file='doc/fig/Fig2A_UncleanedBlockedIndLearningCurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
#   }
#   
#   data <- getBlockedIndividualLearningCurves(group = group, maxppid = maxppid, location = location, targetno = targetno)
#   
#   plot(NA, NA, xlim = c(0,16), ylim = c(-200,200), 
#        xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
#        main = "Individual Learning Curves by Block", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#   abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   abline(h = 0, col = 8, lty = 2)
#   axis(1, at=c(1:15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
#   axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200)) #tick marks for y axis
#   
#   
#   participants <- unique(data$participant)
#   linetypeidx <- 1
#   
#   #library(RColorBrewer)
#   #all palette available from RColorBrewer
#   #display.brewer.all()
#   #we will select the first n colors in the Set1 palette, depending on how many pp we have
#   cols<-brewer.pal(n=maxppid+1,name="Set1")
#   #cols contain the names of n different colors
#   colidx <- 1
#   
#   
#   for (pp in participants){
#     row.idx <- which(data$participant == pp)
#     lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = linetypeidx, col = cols[colidx])
#     points(data$trial[row.idx],data$reachdev[row.idx], pch = 19, col = cols[colidx])
#     
#     linetypeidx <- linetypeidx + 1
#     colidx <- colidx +1
#   }
#   
#   #legend(12,-100,legend=c('Implicit 30°','Strategy 30°','Cursor Jump', 'Hand View'),
#    #      col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
#      #     lty=1,bty='n',cex=1)
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
#  
# }

#from the function above, we see that one participant seems to have "anti-learned", so we remove them then try to plot a mean for all participants
plotBlockedIndLC <- function(group, maxppid, location, targetno, perturb, target='inline'){
  
  if (perturb == 'ROT'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/Fig3_ROT_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=16, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualLearningCurves(group = group, maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    #remove pp004 because they anti-learned
    #data <- subset(data, participant != 'pp4')
    # data <- subset(data, participant != 'pp0')
    # data <- subset(data, participant != 'pp1')
    
    plot(NA, NA, xlim = c(0,16), ylim = c(-200,210), 
         xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Visuomotor rotation", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = '#000000', lty = 2)
    axis(1, at=c(1, 5, 10, 15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
    
    
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
    
    # col <- colourscheme[[perturb]][['S']]
    # lines(c(1:length(allmeans)),allmeans[,1], lwd = 2, lty = 1, col = col)
    # points(c(1:length(allmeans)),allmeans[,1], pch = 19, col = col)
    
    #legend(12,-100,legend=c('Implicit 30°','Strategy 30°','Cursor Jump', 'Hand View'),
    #      col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
    #     lty=1,bty='n',cex=1)
  } else if (perturb == 'MIR'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/Fig3_MIR_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=16, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualLearningCurves(group = group, maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    #remove pp004 because they anti-learned
    #data <- subset(data, participant != 'pp4')
    # data <- subset(data, participant != 'pp0')
    # data <- subset(data, participant != 'pp1')
    
    plot(NA, NA, xlim = c(0,16), ylim = c(-200,210), 
         xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Mirror reversal", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = '#000000', lty = 2)
    axis(1, at=c(1, 5, 10, 15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
    
    
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
    # 
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

plotROTMIRLC <- function(groups = c('noninstructed'), noninstmax = 15, instmax = 31, location = 'maxvel', targetno = 6, target = 'inline'){
  
  #fix titles to include instructed and noninstructed in title
  #instmax and noninstmax will differ depending on maximum pp id number in data
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_BlockedIndLearningCurve.svg', width=18, height=7, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  
  par(mfrow = c(1,2))
  
  for(group in groups){
  #   if(group == 'noninstructed'){
      plotBlockedIndLC(group=group, maxppid=noninstmax, location =location, targetno = targetno, perturb = 'ROT')
      plotBlockedIndLC(group=group, maxppid=noninstmax, location =location, targetno = targetno, perturb = 'MIR')
  #   } else if (group == 'instructed'){
  #     plotBlockedIndLC(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'ROT')
  #     plotBlockedIndLC(group=group, maxppid=instmax, location =location, targetno = targetno, perturb = 'MIR')
  #   }
  }
  

  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Learning Curves STATS----
#Stats are currently only for Non-instructed group

getLearningCurvesLongFormat <- function(groups = c('noninstructed','instructed'), maxppid = 15, location = 'maxvel'){
  
  for (group in groups){
    #Rotation data
    ROTdat <- getROTGroupLearningCurves(group=group,maxppid=maxppid,location=location)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(ROTdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')

    ROTdat <- as.data.frame(ROTdat)
    perturb <- rep('ROT', nrow(ROTdat))
    ROTdat <- cbind(ROTdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longROTdata <- gather(ROTdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longROTdata, file=sprintf('data/ROT_%s_learningcurves_long.csv', group), row.names = F)
    
    #Mirror data
    MIRdat <- getMIRGroupLearningCurves(group=group,maxppid=maxppid,location=location)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(MIRdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    MIRdat <- as.data.frame(MIRdat)
    perturb <- rep('MIR', nrow(MIRdat))
    MIRdat <- cbind(MIRdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longMIRdata <- gather(MIRdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longMIRdata, file=sprintf('data/MIR_%s_learningcurves_long.csv', group), row.names = F)
  }
}

getBlockedLearningCurvesAOV <- function(perturbations = c('ROT','MIR'), group = 'noninstructed', blockdefs) {
  #function reads in learningcurves_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  #to include instructed group, just create another for loop here
  for (perturb in perturbations){  
    curves <- read.csv(sprintf('data/%s_noninstructed_learningcurves_long.csv',perturb), stringsAsFactors=FALSE)  
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

learningCurvesANOVA <- function() {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6)) #6 trials per block
  
  LC4aov <- getBlockedLearningCurvesAOV(blockdefs=blockdefs)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=c(perturbtype, block),type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}
#there is a main effect of block, and a block x perturbtype interaction

learningcurveComparisonMeans <- function(){
  
  #can plot interaction just to eyeball it:
  plot(interactionMeans(lm(compensation ~ block * perturbtype, data=LC4aov), factors=c('perturbtype', 'block'), atx='block'))
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6))
  
  LC4aov <- getBlockedLearningCurvesAOV(blockdefs=blockdefs) 
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

learningcurveComparisonsAllBlocks <- function(method='bonferroni'){
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(85,6))
  
  LC4aov <- getBlockedLearningCurvesAOV(blockdefs=blockdefs) 
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

#pending question of whether this analysis is correct. We already know that variability between rot and mir is different.
#This is further supported by a levene's test of compensation~perturbtype which is significant.
#emmeans assumes that assumptions are met, so it might be using a wrong model for posthoc comparisons.
#what is an appropriate test?

#Learning Curves STATS WITHOUT NEAR TARGET----
#Stats are currently only for Non-instructed group

getLearningCurvesLongFormatWONear <- function(groups = c('noninstructed','instructed'), maxppid = 15, location = 'maxvel'){
  
  for (group in groups){
    #Rotation data
    ROTdat <- getROTGroupLearningCurvesWONear(group=group,maxppid=maxppid,location=location)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(ROTdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    ROTdat <- as.data.frame(ROTdat)
    perturb <- rep('ROT', nrow(ROTdat))
    ROTdat <- cbind(ROTdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longROTdata <- gather(ROTdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longROTdata, file=sprintf('data/ROT_%s_learningcurves_long_wonear.csv', group), row.names = F)
    
    #Mirror data
    MIRdat <- getMIRGroupLearningCurvesWONear(group=group,maxppid=maxppid,location=location)
    ppcols <- c('p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    colnames(MIRdat) <- c('trial', 'p0','p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15')
    
    MIRdat <- as.data.frame(MIRdat)
    perturb <- rep('MIR', nrow(MIRdat))
    MIRdat <- cbind(MIRdat, perturb)
    #gather(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    longMIRdata <- gather(MIRdat, participant, compensation, ppcols[1:length(ppcols)], factor_key=TRUE)
    write.csv(longMIRdata, file=sprintf('data/MIR_%s_learningcurves_long_wonear.csv', group), row.names = F)
  }
}

getBlockedLearningCurvesAOVWONear <- function(perturbations = c('ROT','MIR'), group = 'noninstructed', blockdefs) {
  #function reads in learningcurves_long.csv file then creates a df with cols participant, block, reachdev
  LCaov <- data.frame()
  #to include instructed group, just create another for loop here
  for (perturb in perturbations){  
    curves <- read.csv(sprintf('data/%s_noninstructed_learningcurves_long_wonear.csv',perturb), stringsAsFactors=FALSE)  
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

learningCurvesANOVAWONear <- function() {
  
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(55,6)) #6 trials per block
  
  LC4aov <- getBlockedLearningCurvesAOV(blockdefs=blockdefs)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=compensation, within=c(perturbtype, block),type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}
#there is a main effect of block, and a block x perturbtype interaction

learningcurveComparisonMeansWONear <- function(){
  
  #can plot interaction just to eyeball it:
  plot(interactionMeans(lm(compensation ~ block * perturbtype, data=LC4aov), factors=c('perturbtype', 'block'), atx='block'))
  
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(55,6))
  
  LC4aov <- getBlockedLearningCurvesAOVWONear(blockdefs=blockdefs) 
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

learningcurveComparisonsAllBlocksWONear <- function(method='bonferroni'){
  #styles <- getStyle()
  blockdefs <- list('first'=c(1,6),'second'=c(7,6),'last'=c(55,6))
  
  LC4aov <- getBlockedLearningCurvesAOVWONear(blockdefs=blockdefs) 
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

#Findings are pretty much the same, except that difference in block 1 and 2 for mirror is not quite significant p = .07
#The trend that mirror is higher by second block is kept consistent however.
