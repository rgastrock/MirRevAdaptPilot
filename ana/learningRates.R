library(tidyr)
library(RColorBrewer)
library(svglite)

#Generic Functions----
getParticipantTaskData <- function(id, taskno, task) {
  
  if (id < 10){
    filepath <- sprintf('data/pilot/SELECTED/p00%d/p00%d-%d-%s_selected.txt', id, id, taskno,task) #creates the file path relative to current directory
  } else{
    filepath <- sprintf('data/pilot/SELECTED/p0%d/p0%d-%d-%s_selected.txt', id, id, taskno,task) #creates the file path relative to current directory
  }
  
  df <- read.table(file = filepath) #these files need headers to be added
  # count the number of columns
  CountCol <- ncol(df)
  
  
  # 19 columns, use these as headers
  if (CountCol == 19) {
    colnames(df) <- c("cursorx_cm", "cursory_cm", "homex_cm", "homey_cm", "mousex_cm", "mousey_cm",
                      "participant", "rotation", "step", "targetangle_deg", "targetx_cm", "targety_cm",
                      "time_ms", "trial", "trialselected_bool", "sampleselected_bool", "sampleinterpolated_bool",
                      "maxvelocity_idx", "unsure")
  }
  return(df)
}

rotateTrajectory <- function(X,Y,angle) {
  
  # create rotation matrix to rotate the X,Y coordinates
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix as well
  coordinates <- matrix(data=c(X,Y),ncol=2)
  
  # rotate the coordinates
  Rcoordinates <- coordinates %*% R
  
  # return the rotated reach
  return(Rcoordinates)
  
}

getBSConfidenceInterval <- function(data, resamples) {
  
  data <- data[which(is.finite(data))] #need is.finite due to NA values
  
  #bootstrap to 95% CI with replacement (done when normal t-distribution is not assumed)
  #so multiplies data times 1000 and replaces the values
  samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
  #apply mean function to this new matrix
  BS <- apply(samplematrix, c(1), FUN=mean) #use mean instead of median (we mainly use mean for analyses, even though median is robust to outliers)
  #95% confidence that data falls within range
  #2.5% to 97.5%, with 50% being the 'median mean', which is close to actual mean
  return(quantile(BS, probs = c(0.025, 0.50, 0.975)))
  
}

t.interval <- function(data, variance = var(data, na.rm = TRUE), conf.level = 0.95) {
  #same as getConfidenceInterval, but assumes a normal t-distribution
  
  z <- qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
  
  xbar <- mean(data, na.rm = TRUE)
  sdx <- sqrt(variance/length(data))
  
  return(c(xbar - z * sdx, xbar, xbar + z * sdx)) 
  
}

getColourScheme <- function(perturb = c('ROT', 'MIR')){
  #create a list containing the colourscheme per group
  for (ptype in perturb){
    colourscheme <- list()
    
    # colourscheme[['30implicit']] <- list('S'='#ff8200ff', # pure orange
    #                                      'T'='#ff82002f')    #2f gives a lighter shade of the color
    
    colourscheme[['ROT']] <- list('S'='#e51636ff', #vivid/york red
                                         'T'='#e516362f')
    
    # colourscheme[['cursorjump']] <- list('S'='#c400c4ff', #strong magenta
    #                                      'T'='#c400c42f')
    
    colourscheme[['MIR']] <-   list('S'='#005de4ff', #pure blue
                                         'T'='#005de42f')
    
  }
  return(colourscheme)
}
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

getROTParticipantLearningCurve <- function(id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
    alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
    
    if (id%%2 == 1){
      #mirror then rotation if odd id
      rotatedTraining <- getParticipantTaskData(id, taskno = 11, task = 'rotation')
    } else if (id%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      rotatedTraining <- getParticipantTaskData(id, taskno = 5, task = 'rotation')
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
    #reachdeviations are all negative values, we just multiply by -1 to make percentage positive
    #we will still get some negative values because of some that go below 0%, but the direction of means if plotted will make more sense
    RT$reachdev <- ((RT$reachdev * -1)/30)*100
    
    #use below for absolute errors:
    #so we subtract rotation size (30deg) from all reach deviations
    #RT$reachdev <- (RT$reachdev * -1) - 30 #if we want negative values
    #RT$reachdev <- RT$reachdev - 30 #if we want positive values
    return(RT)
}

getMIRParticipantLearningCurve <- function(id, location){
  #same as rotation, we look into percentage of compensation, but note that magnitude to compensate differs per target
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(id, taskno = 5, task = 'mirror')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(id, taskno = 11, task = 'mirror')
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
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
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

getROTGroupLearningCurves <- function(maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #each column is a participant, but I do not label columns according to participant ID
  participants <- seq(0,maxppid,1)
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppangles <- getROTParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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

getMIRGroupLearningCurves <- function(maxppid, location) { # add angle?
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #each column is a participant, but I do not label columns according to participant ID
  participants <- seq(0,maxppid,1)
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppangles <- getMIRParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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

getROTGroupConfidenceInterval <- function(maxppid, location, type){
  #for (group in groups){
    # get the confidence intervals for each trial of each group
    data <- getROTGroupLearningCurves(maxppid = maxppid, location = location)
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
      write.csv(confidence, file='data/ROT_CI_learningcurve.csv', row.names = F)
    }
  #}
}

getMIRGroupConfidenceInterval <- function(maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  #compangle <- c(15,30,45)
  #for (comp in compangle){
    data <- getMIRGroupLearningCurves(maxppid = maxppid, location = location) #angle = comp
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
      #write.csv(confidence, file=sprintf('data/MIR_%d_CI_learningcurve.csv', comp), row.names = F)
      write.csv(confidence, file='data/MIRtest_CI_learningcurve.csv', row.names = F)
    }
  #}
}

plotLearningCurves <- function(perturb = c('ROT','MIR'),target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_learningcurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  
  for (ptype in perturb) {
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_CI_learningcurve.csv', ptype))
    
    colourscheme <- getColourScheme(perturb=ptype)
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
  legend(70,-150,legend=c('Rotation','Mirror Reversal'),
         col=c(colourscheme[['ROT']][['S']],colourscheme[['MIR']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Individual data: Learning Curves----

#first split 90 trials into sets of 6 trials each
#then in each set, plot individual data as lines

getBlockedIndividualLearningCurves <- function(maxppid, location, targetno){
  
  data <- getMIRGroupLearningCurves(maxppid = maxppid, location = location)
  
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

plotBlockedIndLC <- function(maxppid, location, targetno, target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  data <- getBlockedIndividualLearningCurves(maxppid = maxppid, location = location, targetno = targetno)
  
  plot(NA, NA, xlim = c(0,16), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Individual Learning Curves by Block", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1:15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200)) #tick marks for y axis
  
  
  participants <- unique(data$participant)
  linetypeidx <- 1
  
  #library(RColorBrewer)
  #all palette available from RColorBrewer
  #display.brewer.all()
  #we will select the first n colors in the Set1 palette, depending on how many pp we have
  cols<-brewer.pal(n=maxppid+1,name="Set1")
  #cols contain the names of n different colors
  colidx <- 1
  
  
  for (pp in participants){
    row.idx <- which(data$participant == pp)
    lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = linetypeidx, col = cols[colidx])
    points(data$trial[row.idx],data$reachdev[row.idx], pch = 19, col = cols[colidx])
    
    linetypeidx <- linetypeidx + 1
    colidx <- colidx +1
  }
  
  #legend(12,-100,legend=c('Implicit 30°','Strategy 30°','Cursor Jump', 'Hand View'),
   #      col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
     #     lty=1,bty='n',cex=1)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
 
}

#from the function above, we see that one participant seems to have "anti-learned", so we remove them then try to plot a mean for all participants
plotCleanedBlockedIndLC <- function(maxppid, location, targetno, target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2A_CleanedBlockedIndLearningCurve.svg', width=12, height=7, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  data <- getBlockedIndividualLearningCurves(maxppid = maxppid, location = location, targetno = targetno)
  data <- subset(data, participant != 'pp4')
  
  plot(NA, NA, xlim = c(0,16), ylim = c(-200,200), 
       xlab = "Block", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Individual Learning Curves by Block", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1:15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
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
  
  
  for (pp in participants){
    row.idx <- which(data$participant == pp)
    lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = 'grey')
    points(data$trial[row.idx],data$reachdev[row.idx], pch = 19, col = 'grey')
    
    #linetypeidx <- linetypeidx + 1
    #colidx <- colidx +1
  }
  
  #then create a mean for all, according to trial number
  blockno <- unique(data$trial)
  allmeans <- data.frame()
  for (block in blockno){
    row.idx <- which(data$trial == block)
    blockmean <- data$reachdev[row.idx]
    val <- mean(blockmean, na.rm = TRUE)
    
    if (prod(dim(allmeans)) == 0){
      allmeans <- val
    } else {
      allmeans <- rbind(allmeans, val)
    }
  }
  
  lines(c(1:length(allmeans)),allmeans[,1], lwd = 2, lty = 1, col = 'red')
  points(c(1:length(allmeans)),allmeans[,1], pch = 19, col = 'red')
  
  #legend(12,-100,legend=c('Implicit 30°','Strategy 30°','Cursor Jump', 'Hand View'),
  #      col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
  #     lty=1,bty='n',cex=1)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Reach Aftereffects -----







#Reaction Time acros Blocks -----